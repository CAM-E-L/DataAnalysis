Welcome to the word2vec Python Code:
===============================================

Module created by [Michael Gorki](https://github.com/mgorki)

<br>


# Overview
To run the Python source code, you have two options: **without** or **with** a *graphical user interface* (GUI):

## *Without* GUI
1. Make sure you have *python* and the libraries *spaCy* and *pandas* installed on your computer.
2. Download the file with the list of summarized words in the *Shiny App*. 
3. Install **or** download the language-model for *spaCy* you want to use if not already done before. For further information and language models and trained pipelines you can download have a look at the [*spaCy* documentation on models](https://spacy.io/models)
4. Download the python code (*word2vec.py*) (https://github.com/FennStatistics/CAMtools_CAMapp/blob/main/Python_word2vec/w2vec.py)    
5. Set the Config Variables *nlp*, *LOADFILE* and *SAVEFILE* in the python code. **Savefiles of the same name and location specified will be overwritten if not specified oterwise** 
7. Run the Python code.
8. Upload the file it created to the *Shiny App* module

## *With* GUI
1. Make sure you have *python* and the libraries *spaCy*, *pandas* and *PySimpleGUI* installed on your computer.
2. Download the file with the list of summarized words in the *Shiny App*. 
3. Make sure you have a *spaCy* language-model-pipeline for the language you want to use saved on your machine. For language models and trained pipelines you can download have a look at the *spaCy* documentation on [models](https://spacy.io/models), for information on how to save them on your machine have a look at the *spaCy* documentation on [saving and loading](https://spacy.io/usage/saving-loading)   
4. Download the python code [*word2vecGUI.py*](https://github.com/FennStatistics/CAMtools_CAMapp/blob/main/Python_word2vec/w2vecGUI.py) and [*GUI.py*](https://github.com/FennStatistics/CAMtools_CAMapp/blob/main/Python_word2vec/GUI.py) and save them in the same folder.
5. Run *word2vecGUI.py*.
6. Follow the instructions that are showing up on your screen in the GUI.
7. Upload the file you just created to the *Shiny App* module.


# Details and ressources
1. How to install libraries for python: https://docs.python.org/3/installing/index.html
2. How to run a python (.py) file once you have installed python and libraries for python (there are more ways) on [www.pythonbasics.org](https://pythonbasics.org/execute-python-scripts/) 
3. The module for downloading the list of summarized words in the *Shiny App* is part of the preprocessing part and is named "apply word2vec model". 
...



Thank you for checking out our tools! We’d love to hear from you! Feel free to contact us anytime. 

E-Mail: <cam.contact@drawyourminds.de>

Best,

CAM-Tools Team