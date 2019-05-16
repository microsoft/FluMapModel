import argparse
import json
import os
from multiprocessing import Pool, cpu_count
from tqdm import tqdm
import requests


def get_models(base_url):
    response = requests.get(f'{base_url}/v1/generic_models')
    if response.status_code == 200:
        models = response.json()
        return models
    return []


def test_models(base_url, models):

    with Pool(cpu_count()) as p:
        running_jobs = []
        for model in tqdm(models):
            query = json.loads(model['query_str'])
            running_jobs.append(p.apply_async(test_model, (base_url, query, model)))
            running_jobs.append(p.apply_async(test_model, (base_url, query, model, 'text/csv')))
        for j in tqdm(running_jobs):
            j.wait()
        p.close()


def test_model(base_url, query, model, format='json'):
    headers = dict()
    if format != 'json':
        headers['accepts'] = format
    response = requests.post(f'{base_url}/v1/query', json=query)
    if response.status_code != 200 or len(response.text) < 20:
        print(f'Model {model["id"]} failed')
    else:
        print(f'{model["id"]}: { len(response.text) }')


if __name__ == "__main__":

    default_model_store_path = os.path.abspath(os.path.join(os.path.os.getcwd(), '../../test_model_store'))
    parser = argparse.ArgumentParser(description='Uploads trained SF Models to production')
    parser.add_argument("--api-url", default="http://40.112.165.255/",
                        help="URL for Seattle FLU API Incidence Mapper Model Server API")
    parser.set_defaults(latest=True)
    args = parser.parse_args()
    models = get_models(args.api_url)

    test_models(args.api_url, models)