from flask import Flask, render_template, request
from PIL import Image
import numpy as np
import re
import os
import base64
import boto3
import requests
from botocore.exceptions import ClientError
# from load import *

# Declare a flask app
app = Flask(__name__)
img_width, img_height = 10, 10
BUCKET_NAME = 'haskell-nn'
FILE_NAME = 'output.png'

import base64

def upload_file(file_name, bucket, object_name=None):
    """Upload a file to an S3 bucket

    :param file_name: File to upload
    :param bucket: Bucket to upload to
    :param object_name: S3 object name. If not specified then file_name is used
    :return: True if file was uploaded, else False
    """

    s3 = boto3.client('s3')
    s3.upload_file(
        file_name, bucket, object_name, 
        ExtraArgs={'ACL': 'public-read'}
    )

def parseImage(imgData):
    # parse canvas bytes and save as output.png
    imgstr = re.search(b'base64,(.*)', imgData).group(1)

    with open(FILE_NAME,'wb') as output:
        output.write(base64.b64decode(imgstr))

@app.route('/')
def index():
    # Main page
    return render_template('index.html')

@app.route('/predict', methods=['GET', 'POST'])
def predict():
    # get data from drawing canvas and save as image
    parseImage(request.get_data())  

    # upload the file to s3 bucket
    upload_file(FILE_NAME, BUCKET_NAME, FILE_NAME)

    # define the url 
    payload = {'filepath': 'https://haskell-nn.s3.us-east-2.amazonaws.com/' + FILE_NAME}
    # use POST to send request to Haskell Scotty/WARP server
    res = requests.post('http://localhost:5000/prediction', params=payload)

    return (res.text)

if __name__ == '__main__':
    app.run(host='0.0.0.0', port = 3000, debug=True)