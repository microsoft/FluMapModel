import json
import hashlib

from connexion import ProblemException




class ModelExecutionException(ProblemException):
    def __init__(self, title=None, **kwargs):
        super(ModelExecutionException, self).__init__(title=title, **kwargs)





def get_model_id(query):
    if type(query) is str:
        query = json.loads(query)
    pathogen = query['pathogen'] if 'pathogen' in query else ["all"]
    observed = [x for x in query['observed'] if x not in ['pathogen', 'n', 'catchment', 'positive']]
    query_json = dict(model_type=query['model_type'],
                      observed=sorted(observed, key=str.lower),
                      pathogen=pathogen)
    if 'spatial_domain' in query:
        query_json['spatial_domain'] = query['spatial_domain']
    json_str = json.dumps(query_json, sort_keys=True, separators=(',', ':'))
    m = hashlib.md5()
    m.update(json_str.encode('ascii'))
    return m.hexdigest()