from flask import Flask, render_template, request
from PIL import Image
import numpy as np
import keras.models
import re
import base64
# from load import *
from HaPy import Main

# Declare a flask app
app = Flask(__name__)
img_width, img_height = 10, 10
output_file = 'output.png'

import base64

    
def parseImage(imgData):
    # parse canvas bytes and save as output.png
    imgstr = re.search(b'base64,(.*)', imgData).group(1)
    with open(output_file,'wb') as output:
        output.write(base64.b64decode(imgstr))

def transform():
    """
        Transforms the .png output image by converting it to greyscale, 
        8-bit image, and then rescaling it to the same size as in the NN. 
        It then converts it to a numpy array. 
    """
    # read parsed image back in 8-bit, black and white mode (L), and resize
    img = Image.open(output_file).convert('L').resize((img_width, img_height))
    # convert Image object to numpy array
    img_numpy = np.array(img, dtype=int)
    # reshape image data for use in neural network
    img_final = img_numpy.reshape(img_width*img_height).tolist()

    return img_final

@app.route('/')
def index():
    # Main page
    return render_template('index.html')

@app.route('/predict/', methods=['GET', 'POST'])
def predict():
    # get data from drawing canvas and save as image
    parseImage(request.get_data())  

    # transform the output image
    img = transform()

    # predict with the canvas image
    response = Main.predict(img)
    print (response)
    return response
    # with graph.as_default():
    #     out = model.predict(x)
    #     print(out)
    #     print(np.argmax(out, axis=1))
    #     response = np.array_str(np.argmax(out, axis=1))
    #     return response 

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=9091)