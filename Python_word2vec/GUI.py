import PySimpleGUI as sg
import os.path

## Messages ##
msgLoadingWindow = "Loading input file"
msgLoadingFolder = "Choose the folder where the file from the CAM-App is stored"
msgSavingFolder = "Choose a folder for saving the output"

msgSavingWindow = "Defining output file"
msgLoadingFilename = "Specify the name of the file (without the ending .txt)"
msgSavingFilename = "Choose a filename (files of the same name will be overwritten). The ending .txt will be created automatically"

msgLoadingModel = "Choose the folder where your language model file is stored"


def MissingPath():
    layout = [[sg.Text("Path (and/or filename) not propperly chosen")], [sg.Button("OK")]]

    # Create the window
    window = sg.Window("Error", layout)

    # Create an event loop
    while True:
        event, values = window.read()
        # End program if user closes window or
        # presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()

def ChooseFolder(msgPath, msgFilename, msgWindow):
    # First the window layout in 2 columns

    folder_list_column= [
        [
            sg.Text(msgPath),
            sg.In(size=(25, 1), enable_events=True, key="-FOLDER-"),
            sg.FolderBrowse(),
        ]
    ]

    # For now will only show the name of the file that was chosen

    file_name_column = [
        [sg.Text(msgFilename)],
        #[sg.Text(size=(40, 1), key="-TOUT-")],
        [sg.In(size=(25, 1), enable_events=True, key="-FILENAME-")]
    ]


    # ----- Full layout -----

    layout = [
        [
            sg.Column(folder_list_column),
            sg.VSeperator(),
            sg.Column(file_name_column),
        ],
        [
            sg.Button("OK")
        ]
    ]


    window = sg.Window(msgWindow, layout)


    while True:
        event, values = window.read()
        if event == "Exit" or event == sg.WIN_CLOSED:
            break

        # Folder name was filled in, make a list of files in the folder
        elif event == "-FOLDER-":
            folder = values["-FOLDER-"]
        
        elif event == "-FILENAME-":
            filename = values["-FILENAME-"]        

        elif event == "OK":
            try:  
                return(str(folder + '/' + filename +'.txt'))
            except:
                MissingPath()
                            

    window.close()


def End(Speicherpfad):
    layout = [[sg.Text(("Module completed. Output saved to %s"%(str(Speicherpfad))))], [sg.Button("OK")]]

    # Create the window
    window = sg.Window("Finished", layout)

    # Create an event loop
    while True:
        event, values = window.read()
        # End program if user closes window or
        # presses the OK button
        if event == "OK" or event == sg.WIN_CLOSED:
            break

    window.close()



def definePath(operation):
    if operation == "loading":
        path = ChooseFolder(msgLoadingFolder, msgLoadingFilename, msgLoadingWindow)
    else:
        path = ChooseFolder(msgSavingFolder, msgSavingFilename, msgSavingWindow)

    try:
        print(path)
        return path
    except:
        print("An error occurred")
        exit


def ChooseModel(msgPath=msgLoadingModel):
    # First the window layout in 1 column

    folder_list_column= [
        [
            sg.Text(msgPath),
            sg.In(size=(25, 1), enable_events=True, key="-FOLDER-"),
            sg.FolderBrowse(),
        ]
    ]

    # ----- Full layout -----

    layout = [
        [
            sg.Column(folder_list_column),
        ],
        [
            sg.Button("OK")
        ]
    ]


    window = sg.Window("Language model folder", layout)


    while True:
        event, values = window.read()
        if event == "Exit" or event == sg.WIN_CLOSED:
            break

        # Folder name was filled in, make a list of files in the folder
        elif event == "-FOLDER-":
            folder = values["-FOLDER-"]
           

        elif event == "OK":
            try:  
                return(str(folder))
            except:
                MissingPath()
                            

    window.close()