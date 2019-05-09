import argparse
import csv
import requests
import os
from tqdm import tqdm


def upload_model(model, api_url, models_path, api_key):
    headers = {'X-Auth': api_key}
    model_path = os.path.join(models_path, f"{model['filename']}.csv")
    model_data = {
        "id": model['filename'],
        "name": model['name'],
        "query_str": model['queryJSON'],
        "model_type": model['type'],
        "created": model['created'],

    }
    files = {
        'model': open(model_path, 'rb')
    }

    r = requests.post(api_url, data=model_data, headers=headers, files=files)
    if r.status_code != 201:
        raise Exception("upload failed")


def get_models(filename):
    rows = []
    with open(filename) as tsvfile:
        reader = csv.DictReader(tsvfile, dialect='excel-tab')
        rows = [row for row in reader]
    return rows


if __name__ == "__main__":

    default_model_store_path = os.path.abspath(os.path.join( os.path.os.getcwd(), 'test_model_store'))
    parser = argparse.ArgumentParser(description='Uploads trained SF Models to production')
    parser.add_argument("--db-file", default=os.path.join(default_model_store_path, "modelDB.tsv"), help="Where the modelDB.tsv produced during training is stored")
    parser.add_argument("--model-store", default=default_model_store_path)
    parser.add_argument("--api-url", default="http://40.112.165.255/v1/pathogen_models", help="URL for Seattle FLU API Incidence Mapper Model Server API")
    parser.add_argument("--api-key", help="API-KEY Allowing uploads")
    
    args = parser.parse_args()

    models = get_models(args.db_file)
    pbar = tqdm(models)
    for model in pbar:
        pbar.set_description(f"Processing {model['name']}")
        upload_model(model, args.api_url, args.model_store, args.api_key)