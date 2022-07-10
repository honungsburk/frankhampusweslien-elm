import base64
import requests
import os
import json
import importlib

environmentMod = input('environment')
importlib.import_module(environmentMod)

def base64File(filePath): 
    with open(filePath, "rb") as img_file:
        return base64.b64encode(img_file.read())

def buildBody(assetName, previewBase64, subFileBase64):
    desc = "Stained Glass is a series of 100 unique generative artworks."
    return {
        "assetName": assetName,
        "previewImageNft": {
            "name": assetName,
            "mimetype": "image/png",
            "fileFromBase64": previewBase64,
            "description": desc,
            "metadataPlaceholder": [
                
            ]
        },
        "subfiles": [
            {
            "name": assetName,
            "mimetype": "image/svg+xml",
            "fileFromBase64": subFileBase64,
            "description" : desc,
            "metadataPlaceholder": [
            ]
            }
        ]
        }

apikey = "*****"
projectId = "****"

url = "https://api.nft-maker.io/UploadNft/" + environment["apikey"] + "/" + projectId

processed = {}

try:
    with open("processed.json", "r") as file:
        processed = json.load(file)
except:
    print("Could not find processed.")

try:

    for svgFile in os.listdir('./public/assets/art/stained-glass/'):
        fileName = os.path.basename(svgFile) 
        name, extention = os.path.splitext(fileName) 
        if name not in processed.keys() and extention == ".svg":
            print("Processing: " + name)
            preview = base64File("public/assets/art/stained-glass/thumbnails/" + name +".png")
            subfile = base64File("public/assets/art/stained-glass/" + name +".svg")
            data = buildBody(name, preview, subfile)

            r = requests.post(url, json=data)
            print(r.status_code)
            processed[name] = r.status_code
            print(r.json())
finally:
    with open("processed.json", "w") as file:
        json.dump(processed, file)