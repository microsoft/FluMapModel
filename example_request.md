### Example Requests

Incidence request

##### Curl
```
curl -X POST \
  http://40.112.165.255/flu \
  -H 'cache-control: no-cache' \
  -H 'content-type: application/json' \
  -H 'postman-token: aa46a1fe-9a61-225e-2416-94b321baa238' \
  -d '{
	"output":["incidence"],
	"attributes":["Census_Tract","vax_status"],
	"basis":["year"],
	"values":["incidence_median","incidence_sd"]
}'
```

##### Node Request
```javascript
var request = require("request");

var options = { method: 'POST',
  url: 'http://40.112.165.255/flu',
  headers: 
   { 'postman-token': 'ed0b5dd6-9eae-30de-a8eb-d6869dc0bf76',
     'cache-control': 'no-cache',
     'content-type': 'application/json' },
  body: 
   { output: [ 'incidence' ],
     attributes: [ 'Census_Tract', 'vax_status' ],
     basis: [ 'year' ],
     values: [ 'incidence_median', 'incidence_sd' ] },
  json: true };

request(options, function (error, response, body) {
  if (error) throw new Error(error);

  console.log(body);
});
```

##### Node Native
```javascript
var options = {
  "method": "POST",
  "hostname": "40.112.165.255",
  "port": null,
  "path": "/flu",
  "headers": {
    "content-type": "application/json",
    "cache-control": "no-cache",
    "postman-token": "619097ad-0469-3593-8f6c-938f1a8c6d7c"
  }
};

var req = http.request(options, function (res) {
  var chunks = [];

  res.on("data", function (chunk) {
    chunks.push(chunk);
  });

  res.on("end", function () {
    var body = Buffer.concat(chunks);
    console.log(body.toString());
  });
});

req.write(JSON.stringify({ output: [ 'incidence' ],
  attributes: [ 'Census_Tract', 'vax_status' ],
  basis: [ 'year' ],
  values: [ 'incidence_median', 'incidence_sd' ] }));
req.end();
```
##### javscript
```javascript
var data = JSON.stringify({
  "output": [
    "incidence"
  ],
  "attributes": [
    "Census_Tract",
    "vax_status"
  ],
  "basis": [
    "year"
  ],
  "values": [
    "incidence_median",
    "incidence_sd"
  ]
});

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === 4) {
    console.log(this.responseText);
  }
});

xhr.open("POST", "http://40.112.165.255/flu");
xhr.setRequestHeader("content-type", "application/json");
xhr.setRequestHeader("cache-control", "no-cache");
xhr.setRequestHeader("postman-token", "43420113-8008-3c53-9cc3-e6c8de9d8d55");

xhr.send(data);
```
##### Python
```python
import requests

url = "http://40.112.165.255/flu"

payload = "{\n\t\"output\":[\"incidence\"],\n\t\"attributes\":[\"Census_Tract\",\"vax_status\"],\n\t\"basis\":[\"year\"],\n\t\"values\":[\"incidence_median\",\"incidence_sd\"]\n}"
headers = {
    'content-type': "application/json",
    'cache-control': "no-cache",
    'postman-token': "d3083842-39d5-c6c5-9332-59f9eadcb234"
    }

response = requests.request("POST", url, data=payload, headers=headers)

print(response.text)
```

### Observed Request

##### Curl
```
curl -X POST \
  http://40.112.165.255/flu \
  -H 'cache-control: no-cache' \
  -H 'content-type: application/json' \
  -H 'postman-token: 42b2d09b-fb4a-8135-8993-f7f8e6fa308c' \
  -d '{

  "output": ["observed"],
  "attributes": ["Census_Tract", "vax_status"],
  "basis": ["year"],
  "values": ["flu_count", "nsamples"]

}
```
##### Node Request
```javascript
var request = require("request");

var options = { method: 'POST',
  url: 'http://40.112.165.255/flu',
  headers: 
   { 'postman-token': '7b6caf7d-a3c6-88cc-1306-249f88d152b3',
     'cache-control': 'no-cache',
     'content-type': 'application/json' },
  body: 
   { output: [ 'observed' ],
     attributes: [ 'Census_Tract', 'vax_status' ],
     basis: [ 'year' ],
     values: [ 'flu_count', 'nsamples' ] },
  json: true };

request(options, function (error, response, body) {
  if (error) throw new Error(error);

  console.log(body);
});

```
##### Node Native
```javascript
var http = require("http");

var options = {
  "method": "POST",
  "hostname": "40.112.165.255",
  "port": null,
  "path": "/flu",
  "headers": {
    "content-type": "application/json",
    "cache-control": "no-cache",
    "postman-token": "18f97827-f7f3-c58c-26f4-5e9a0c04ace2"
  }
};

var req = http.request(options, function (res) {
  var chunks = [];

  res.on("data", function (chunk) {
    chunks.push(chunk);
  });

  res.on("end", function () {
    var body = Buffer.concat(chunks);
    console.log(body.toString());
  });
});

req.write(JSON.stringify({ output: [ 'observed' ],
  attributes: [ 'Census_Tract', 'vax_status' ],
  basis: [ 'year' ],
  values: [ 'flu_count', 'nsamples' ] }));
req.end();
```

##### Javascript
```javascript
var data = JSON.stringify({
  "output": [
    "observed"
  ],
  "attributes": [
    "Census_Tract",
    "vax_status"
  ],
  "basis": [
    "year"
  ],
  "values": [
    "flu_count",
    "nsamples"
  ]
});

var xhr = new XMLHttpRequest();
xhr.withCredentials = true;

xhr.addEventListener("readystatechange", function () {
  if (this.readyState === 4) {
    console.log(this.responseText);
  }
});

xhr.open("POST", "http://40.112.165.255/flu");
xhr.setRequestHeader("content-type", "application/json");
xhr.setRequestHeader("cache-control", "no-cache");
xhr.setRequestHeader("postman-token", "4d81fb6f-f40e-b108-7a42-da98ab8ddd40");

xhr.send(data);
```
##### Python
```python
import requests

url = "http://40.112.165.255/flu"

payload = "{\n\n  \"output\": [\"observed\"],\n  \"attributes\": [\"Census_Tract\", \"vax_status\"],\n  \"basis\": [\"year\"],\n  \"values\": [\"flu_count\", \"nsamples\"]\n\n}\n\n "
headers = {
    'content-type': "application/json",
    'cache-control': "no-cache",
    'postman-token': "86c99e05-61c5-204a-3e58-53db81e23856"
    }

response = requests.request("POST", url, data=payload, headers=headers)

print(response.text)
```