# Handwritten Digit Recognition with a Neural Network in Haskell

The repository contains a neural network written from scratch on Haskell, with accompanying Flask app that allows a user to draw a digit to be predicted on. Some features of the project include: 

1. Feed forward neural network using 1 hidden layer
2. Backpropagation algorithm 
3. Gradient descrent algorithm
4. Image conversion using JuicyPixels and HIP libraries
5. Flask app for web server and interface that uses HTML/JS/CSS

# Usage
Requirements: Python 3, virtualenv, GHC, Stack, Unix  
Haskell Platform (with GHC) available at: https://www.haskell.org/platform/  
Haskell Tool Stack install instructions: https://docs.haskellstack.org/en/stable/README

# Instructions to run Flask app and predict
You must have Stack installed in order to run the Haskell script from within the Flask app. Then follow these steps:
1. Make a virtual environment for python: `python3 -m venv env` . Activate with `source env/bin/activate` .
2. Run requirements.txt. `pip3 install -r requirements.txt`
3. Run flask app. `python3 main.py`
4. Go to `localhost:9091` on your browser to draw and get the prediction.
5. Deactivate virtualenv with command `deactivate`
