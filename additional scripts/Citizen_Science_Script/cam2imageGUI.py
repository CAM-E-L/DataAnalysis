import PySimpleGUI as sg


### Messages (dictionary) ###
messages = {
    "msgLoadingFolder": """Select Folder that contains all CAM-files.
        -> CAM-files can be contained in sub-folders.
        -> Each CAM must be saved as a separate (json/txt) file.""",

    "msgLoadingSVGs": """Select SVG files that should be converted to png format""",

    "msgChooseFilesForRenamingPrefix": """Select files you want to rename by adding a prefix to their filename""",

    "msgChooseJSON": """Select the JSON/txt file that contains the data of more than one CAM""",

    "msgSavingFolder": """Select Folder where results should be saved.
        -> Please mind that only folders (not the files they may contain) are shown on this screen, so make sure you do not overwrite existing files""",

    "msgEnterAdress": """Please enter the exact adress where CAMEL is deployed with researcher buttons (the variable "ShowResearcherButtons" must be set to "true" in the respective config file).
        -> This might be a web-adress (like e.g., https://weblab.uni-xyz.edu/publix/123...) if deployed on a web server or a local adress (like e.g., http://123.0.0.1:5500/), if you are running CAMEL locally on your machine""",

    "msgEnterPrefix": """Please enter the prefix you want to add to your filename(s).
        -> Make sure the resulting filename doesn't already exist unless you want to overwrite an existing file""",

    "msgDecideWhichOperation" : """Please choose which operation you want to perform""",    
    
    "msgDecideCAMs2SVG": """Do you want to create vector graphics (SVGs) from CAM-files?""",

    "msgDecideSVGsConversion": """Do you want to convert SVG vector graphics (like the CAM-graphics) into PNG formate?"""
}


### Text of buttons (dictionary) ###
buttonTexts = {
    "btnGetSVGs": 'Get vector graphics (SVGs) from CAMs',
    "btnConvertSVG2PNG": 'Convert vector graphics (SVGs) into PNG-formate',
    "btnAddPreffix": 'Rename files by adding a preffix to filenames',    
    "btnSplitFile": 'Split a single file containing multiple CAMs into several files with one file per CAM',
    "btnCropSVGs": 'Crop vector graphics (SVGs)'    
}


#####################
### Error Windows ###
#####################

def missingPath():
    layout = [[sg.Text("Path not propperly chosen")], [sg.Button("OK")]]
    window = sg.Window("Error", layout)  # Create the window

    # Create an event loop
    while True:
        event, values = window.read()
        # End program if user closes window or
        # presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()


def invalidFolder():
    layout = [[sg.Text("The folder you specified does not exist/is not valid!")], [sg.Button("OK")]]  
    window = sg.Window("Error", layout)# Create the window

    # Create an event loop
    while True:
        event, values = window.read()
        # End program if user closes window or
        # presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()


def MissingInput():
    layout = [[sg.Text("You entered no input!")], [sg.Button("OK")]]
    window = sg.Window("Error", layout)  # Create the window

    # Create an event loop
    while True:
        event, values = window.read()
        # End program if user closes window or
        # presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()


def chooseFiles(fileTypes=None, msg=messages["msgLoadingSVGs"], multiple=True):
    fileTypes = [("Any type", "*")] if fileTypes == None else tuple([("Any type", "*"), *fileTypes])
    #print(fileTypes)
    files = sg.popup_get_file(msg, multiple_files=multiple, file_types=(fileTypes), title="Select files")
    print(files.split(';'))
    if files == "" or None:
        missingPath()
        chooseFiles(fileTypes=fileTypes, msg=msg, multiple=multiple)
    else:
        if multiple:
            return files.split(';')
        else:
            return files


#################
### Decisions ###
#################

def decideWhichOperation():
    layout = [
        [sg.Text((messages["msgDecideWhichOperation"]))], 
        [sg.Button(buttonTexts["btnGetSVGs"]), sg.Button(buttonTexts["btnConvertSVG2PNG"]), sg.Button(buttonTexts["btnAddPreffix"]), sg.Button(buttonTexts["btnSplitFile"]), sg.Button(buttonTexts["btnCropSVGs"])],
    ] 
    window = sg.Window(title="Choose an operation", layout=layout) # Create the window

    ## Create an event loop
    while True:
        event, values = window.read()
        ## End program if user closes window or presses the OK button
        if (event == buttonTexts["btnGetSVGs"]) or (event == buttonTexts["btnConvertSVG2PNG"]) or (event == buttonTexts["btnAddPreffix"]) or (event == buttonTexts["btnSplitFile"]) or (event == buttonTexts["btnCropSVGs"]) or (event == sg.WIN_CLOSED):
            break
    window.close()
    print(event)
    if event == sg.WIN_CLOSED:
        print("Exited by user")
        return("Exit")  # Exit the whole program
    else: 
        ## Find the key in the buttonTexts dictionary that belongs to the event (the value) and return it
        for key, value in buttonTexts.items(): 
            if event == value:
                return key
    

def decideCAMs2SVG():
    layout = [[sg.Text((messages["msgDecideCAMs2SVG"]))], 
        [sg.Button("Yes"), sg.Button("No")],] 
    window = sg.Window("Convert images?", layout) # Create the window

    ## Create an event loop
    while True:
        event, values = window.read()
        ## End program if user closes window or presses the OK button
        if (event == "Yes") or (event == "No") or (event == sg.WIN_CLOSED):
            break
    window.close()
    if event == "Yes":
        return True
    else:
        return False
    

def decideImageConversion():
    layout = [[sg.Text((messages["msgDecideSVGsConversion"]))], 
        [sg.Button("Yes"), sg.Button("No")],] 
    window = sg.Window("Convert images?", layout) # Create the window

    ## Create an event loop
    while True:
        event, values = window.read()
        ## End program if user closes window or presses the OK button
        if (event == "Yes") or (event == "No") or (event == sg.WIN_CLOSED):
            break
    window.close()
    if event == "Yes":
        return True
    else:
        return False


##########################
### Text-input windows ###
##########################

def enterAdress():
    adress = sg.popup_get_text(messages["msgEnterAdress"], title="Enter adress")
    if adress == "" or None:
        MissingInput()
        enterAdress()
    else:
        print("The chosen adress where CAMEL is deployed is: " + str(adress))
        return str(adress)


def enterPrefix():
    prefix = sg.popup_get_text(messages["msgEnterPrefix"], title="Enter Prefix")
    if prefix == "" or None:
        MissingInput()
        enterPrefix()
    else:
        print("The chosen prefix is: " + str(prefix))
        return str(prefix)


#####################################
### Folder/File selection windows ###
#####################################

def chooseLoadingFolder():
    msg = messages["msgLoadingFolder"]
    folder = sg.popup_get_folder(msg, title="Loading all from folder")
    if folder == "" or None:
        missingPath()
        chooseLoadingFolder()
    else:
        print(folder)
        return folder


def chooseSavingFolder():
    folder = sg.popup_get_folder(messages["msgSavingFolder"], title="Saving to folder")
    if folder == "" or None:
        missingPath()
        chooseSavingFolder()
    else:
        print(folder)
        return folder


#############
### Other ###
#############

def completion(operation=None):
    if operation == None:
        layout = [[sg.Text(("Operation completed."))], [sg.Button("OK")]] 
    else:
        layout = [[sg.Text((str(operation) + " completed."))], [sg.Button("OK")]]

    window = sg.Window("Finished", layout)  # Create the window

    ## Create an event loop
    while True:
        event, values = window.read()
        ## End program if user closes window or presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()

