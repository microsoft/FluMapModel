import importlib
from functools import wraps
from typing import Callable

default_exclude = ['__init__.py']

def singleton_function(func: Callable) -> Callable:
    """
    Allows a function to run once then cache its results for later calls
    :param func: Function to be cached
    :return: Wrapper function
    """

    @wraps(func)
    def wrapper(*args, **kwargs):
        if not hasattr(func, 'has_ran'):
            setattr(func, 'cache_value', func(*args, **kwargs))
            setattr(func, 'has_ran', True)
        return getattr(func, 'cache_value')
    return wrapper

def dynamic_import_all(module):
    # get a handle on the module
    mdl = importlib.import_module(module)

    # is there an __all__?  if so respect it
    if "__all__" in mdl.__dict__:
        names = mdl.__dict__["__all__"]
    else:
        # otherwise we import all names that don't begin with _
        names = [x for x in mdl.__dict__ if not x.startswith("_")]
    return names