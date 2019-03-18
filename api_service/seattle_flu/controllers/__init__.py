import glob
import importlib
from os.path import dirname, isfile, basename

modules = glob.glob(dirname(__file__)+"/*.py")
__all__ = [importlib.import_module('{}.{}'.format(__name__, basename(f)[:-3])) for f in modules if isfile(f) and not f.endswith('__init__.py')]