from typing import Dict
import dramatiq
from subprocess import Popen, PIPE

MODEL_PROCESS: Dict[str, Popen] = {}


def get_model_process(model_id) -> Popen:
    """
    Attempt to load model from cache. If the model is not loaded, initialize it

    At the moment we assume one model exists. Later this will actually do the work of fetching a specific model and
    ensuring we have some type of way to interact with it

    Notes:
        We need to do some better model caching. This caching is PER WORKER THREAD! which could get EXPENSIVE
        We could look at moving to named pipes but that does complicate who is current writing to process.
        One thing we could do is
        Named Pipe:
            <- Request data and a random file name
            -> R Processes request and write to file
            <- We poll for file and then begin streaming as soon as it start to write
    Args:
        model_id: ID Of model to load
    Returns:
        Popen object that points to our R process
    """
    if model_id not in MODEL_PROCESS:
        # Launch R Script
        MODEL_PROCESS[model_id] = Popen(["/usr/bin/R", "--no-save"], stdout=PIPE, stdin=PIPE, encoding='ascii')
        # For now load the script here. We should move this to a generic R script
        # Load our package
        MODEL_PROCESS[model_id].stdin.write('library("predictModelTestPkg")\n')
        # Flush the input
        MODEL_PROCESS[model_id].stdin.flush()
        # Run until we here our library load
        for line in iter(MODEL_PROCESS[model_id].stdout.readline, ''):
            if line.startswith('> library("predictModelTestPkg")'):
                break
    return MODEL_PROCESS[model_id]


@dramatiq.actor(store_results=True)
def r_flu_model_request(model_id, message):
    """
    Process  our R Model Request

    Args:
        model_id: model id,a t moment it is ignored
        message: Message to pass to R

    Returns:
        R Model result as string
    """
    # For now hard code model id. Later we will make this part of message and strip it out
    proc: Popen = get_model_process(model_id)
    proc.stdin.write(f"query <- jsonlite::fromJSON( '{message}' )\n")
    proc.stdin.write("data <- predictModel(query)\n")
    proc.stdin.flush()
    for line in iter(proc.stdout.readline, ''):
        print(line)
        if 'data <- predictModel(query)' in line:
            break

    proc.stdin.write("jsonlite::toJSON( data )\n")
    proc.stdin.flush()
    # skip first line of output and grab second(our json message)
    proc.stdout.readline()
    # This output is HUGE. It would be nice to stream it into result but not sure that is possible
    # Alternativly we could get
    result = proc.stdout.readline()
    return result
