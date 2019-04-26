import argparse
import csv
import requests
from tqdm import tqdm

def upload_model(model, api_url):
    headers = {'Content-type': 'multipart/form-data'}
    files = {
        'smooth': open(f"{model['id']}.tsv", 'rb')
    }
    if model['latent']:
        files['latent'] = open(f"{model['id']}_latent.tsv", 'rb')
    
    model_data = {
        "id": model['id'],
        "name": model['name'],
        "query": model['query'],
        "latent": model['latent'],
        "created": model['created']
    }
    r = requests.post(api_url, files=files, data=model_data, headers=headers)

def get_models(filename):
    rows = []
    with open(filename) as tsvfile:
        reader = csv.DictReader(tsvfile, dialect='excel-tab')
        rows = [row for row in reader]
    return rows

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Uploads trained SF Models to production')
    parser.add_argument("--db-file", default="modelDB.tsv", help="Where the modelDB.tsv produced during training is stored")
    parser.add_argument("--api-url", default="http://40.112.165.255/api/models", help="URL for Seattle FLU API Incidence Mapper Model Server API")
    parser.add_argument("--api-key", help="API-KEY Allowing uploads")
    
    args = parser.parse_args()

    models = get_models(args.db_file)
    pbar = tqdm(models)
    for model in pbar:
        pbar.set_description(f"Processing {model['name']}")
        upload_model(model, args.api_url)