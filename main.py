from flask import Flask, render_template, request
from PIL import Image
import numpy as np
import re
import os
import base64
import requests
import subprocess
from PIL import Image
import PIL.ImageOps

# Declare a flask app
app = Flask(__name__)

FILE_NAME = 'output.png'

def parseImage(imgData):
    # parse canvas bytes for the image 
    imgstr = re.search(b'base64,(.*)', imgData).group(1)

    # write the canvas image
    with open(FILE_NAME,'wb') as output:
        output.write(base64.b64decode(imgstr))

    # open the output canvas image, invert the colors and save it 
    image = Image.open(FILE_NAME)
    inverted_image = PIL.ImageOps.invert(image.convert('RGB'))
    inverted_image.save(FILE_NAME)

@app.route('/')
def index():
    # Main page
    return render_template('index.html')

@app.route('/predict', methods=['GET', 'POST'])
def predict():
    # get data from drawing canvas and save as image
    parseImage(request.get_data())  

    # call haskell script and pass in the model structure file, trained model file, and the filepath to the canvas image
    res = subprocess.check_output(['stack', 'build', '--exec', 'image-exe run sigmoid.cfg trainedSigmoid.cfg output.png'])

    # return the prediction of the canvas image
    return (res)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=9091, debug=True)