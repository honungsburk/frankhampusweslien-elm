import base64
import requests
import os
import json
import importlib
import environment

# environmentMod = input('environment')
# importlib.import_module(environmentMod)

def base64File(filePath): 
    with open(filePath, "rb") as img_file:
        return base64.b64encode(img_file.read())

def makePlaceHolders(metadata):
    placeholders = []
    for key in metadata:
        params = metadata[key]
        if isinstance(params, float) or isinstance(params, int):
            placeholders.append({ "value": str(params), "name" : key})
        else:
            extentions = ["x", "y", "z"]
            i = 0
            for param in params:
                placeholders.append({ "value": str(param), "name" : key + "_" + extentions[i]})
                i = i + 1
    return placeholders

def buildBody(assetName, previewBase64, subFileBase64, parametersJson):
    desc = "Algorithmic Marbling - beauty out of noise."
    parametersJson["seed"] = assetName
    name = assetName.zfill(4)
    return {
        "assetName": name, #Minimum length of names
        "previewImageNft": {
            "name": name,
            "mimetype": "image/png",
            "fileFromBase64": previewBase64,
            "description": desc,
            "metadataPlaceholder": makePlaceHolders(parametersJson)
        },
        "subfiles": [
            {
            "name": name,
            "mimetype": "image/png",
            "fileFromBase64": subFileBase64,
            "description" : desc,
            "metadataPlaceholder": [
            ]
            }
        ],
        }

projectId = "17923"

url = "https://api.nft-maker.io/UploadNft/" + environment.environment["apikey"] + "/" + projectId

processed = { }

try:
    with open("processed.json", "r") as file:
        processed = json.load(file)
except:
    print("Could not find processed.")

try:

    files = os.listdir('./public/assets/art/algo-marble/high-res')
    files.sort()

    for pngFile in files:
        fileName = os.path.basename(pngFile) 
        name, extention = os.path.splitext(fileName) 
        if (name not in processed.keys() or processed[name] != 200) and extention == ".png":
            print("Processing: " + name)
            #print("previous error: " +  str(processed[name]))
            preview = base64File("public/assets/art/algo-marble/low-res/" + name +"_low_res.png")
            subfile = base64File("public/assets/art/algo-marble/high-res/" + name + ".png")
            with open("public/assets/art/algo-marble/parameters/" + name + ".json", "r") as jsonFile:
                parameters = json.load(jsonFile)
                data = buildBody(name, preview, subfile, parameters)

                r = requests.post(url, json=data)
                print(r.status_code)
                processed[name] = r.status_code
                #print(r.json())

finally:
    with open("processed.json", "w") as file:
        json.dump(processed, file)