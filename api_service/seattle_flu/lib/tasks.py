import argparse
import multiprocessing
import sys
from threading import local

import click
import dramatiq
from dramatiq.brokers.rabbitmq import URLRabbitmqBroker
from dramatiq.results import Results
from dramatiq.results.backends import RedisBackend
from flask.cli import AppGroup
from seattle_flu.lib.common import singleton_function

CPUS = multiprocessing.cpu_count()

def dramatiq_parse_arguments():
    parser = argparse.ArgumentParser(
        prog="dramatiq",
        description="Run dramatiq workers.",
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    parser.add_argument(
        "--broker", "-b", default=None
    )

    parser.add_argument(
        "--processes", "-p", default=CPUS, type=int,
        help="the number of worker processes to run (default: %s)" % CPUS,
    )
    parser.add_argument(
        "--threads", "-t", default=8, type=int,
        help="the number of worker threads per process (default: 8)",
    )
    parser.add_argument(
        "--path", "-P", default=".", nargs="*", type=str,
        help="the module import path (default: .)"
    )
    parser.add_argument(
        "--queues", "-Q", nargs="*", type=str,
        help="listen to a subset of queues (default: all queues)",
    )
    parser.add_argument(
        "--pid-file", type=str,
        help="write the PID of the master process to a file (default: no pid file)",
    )
    parser.add_argument(
        "--log-file", type=argparse.FileType(mode="a", encoding="utf-8"),
        help="write all logs to a file (default: sys.stderr)",
    )

    parser.add_argument("--verbose", "-v", action="count", default=0, help="turn on verbose log output")
    return parser.parse_args(sys.argv[3:])


def start_dramatiq_workers(app, processes=None):
    from dramatiq import cli as dm
    args = dramatiq_parse_arguments()
    args.module = None
    args.modules = []
    args.workers = []
    if processes:
        args.processes = int(processes)
    dm.parse_arguments = lambda: args
    args.broker = 'rse_api'
    dm.import_broker = lambda x: ('rse_api', app.broker)
    dm.main(args)


class AppContextMiddleware(dramatiq.Middleware):
    state = local()

    def __init__(self, app):
        self.app = app

    def before_process_message(self, broker, message):
        context = self.app.app_context()
        context.push()

        self.state.context = context

    def after_process_message(self, broker, message, *, result=None, exception=None):
        try:
            context = self.state.context
            context.pop(exception)
            del self.state.context
        except AttributeError:
            pass

    after_skip_message = after_process_message


@singleton_function
def get_broker(app):
    broker_url = app.config.get('RABBIT_URI', None)
    app.logger.info('Connecting to Rabbit MQ @ {}'.format(broker_url))
    broker = URLRabbitmqBroker(broker_url)

    if broker is not None:
        dramatiq.set_broker(broker)
        broker.add_middleware(AppContextMiddleware(app))
        get_worker_cli(app)
    return broker


@singleton_function
def get_worker_cli(app):
    # Get flask db should have been called before this with any setup needed
    worker_cli = AppGroup('workers', help="Commands related to workers")

    @worker_cli.command('start', help="Starts all the workers including corn")
    @click.option('--cron', default=True, help='Whether we want to run cron jobs as well')
    @click.option('--processes', default=None, help='Whether we want to run cron jobs as well')
    def start_workers(cron, processes):
        start_dramatiq_workers(app, processes)

    @worker_cli.command('list', help="Lists all the workers")
    def list_workers():
        workers = dramatiq.get_broker().get_declared_actors()
        workers = sorted(workers)
        print('Workers available: ')
        [print(worker) for worker in workers]

    app.cli.add_command(worker_cli)

    return worker_cli


@singleton_function
def default_dramatiq_setup_result_backend(app, broker):
    backend_url = app.config.get('REDIS_URI', None)
    result_backend = Results(backend=RedisBackend(url=backend_url))
    broker.add_middleware(result_backend)
    return result_backend