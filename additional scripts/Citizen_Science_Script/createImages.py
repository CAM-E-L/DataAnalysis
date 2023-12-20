import os
import pathlib
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.wait import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.firefox.options import Options
import cairosvg 
from lxml import etree


### Opens Firefox, opens the local or web-adress (adress parameter) in the browser and sets the browsers download path (pathOut parameter)  
def initBrowser(adress, pathOut):
    try:
        ### Browser Settings###
        options = Options()
        options.set_preference("browser.download.folderList", 2)
        options.set_preference("browser.download.manager.showWhenStarting", False)
        options.set_preference("browser.download.dir", pathOut)

        driver = webdriver.Firefox(options=options)  # Using Firefox. Use other browser that is supported by CAMEL if you want
        driver.get(adress)  # Opening CAMEL in the browser

        return driver
    except:
        print("Initializing browser failed! Crashing now,")
        return "fatal error"


### Creates SVGs from CAM files, takes as parameters (1.) the browser where CAMEL is opened and (2.) the CAM files
def createSVGs(driver, filesIn):
    for file in filesIn:
        ### Loading the CAM-file into CAMEL ###
        try:
            upDownButtons = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, 'hideResearcherButtonsTop')))  # Find the bar with the upload and download button (wait till propperly loaded, timeout=10sec)
            uploadButton = upDownButtons.find_element(by=By.ID, value='fileToLoad')  # Find the 'Upload CAM from file' button 
        except:
            print("Upload button could NOT be found!")

        try:
            uploadButton.send_keys(file)
            print("the file " + str(file) + " was loaded")
        except:
            print("There was an error loading " + str(file))

        ### Saving the vector graphic (svg) file of the CAM ###
        try:
            rightButtons = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, 'rightButton')))  # Find the bar that contains (among others) the download CAM as picture button (wait till propperly loaded, timeout=10sec)
            pictureButton = WebDriverWait(driver, 30).until(EC.element_to_be_clickable((rightButtons.find_element(by=By.ID, value='saveCAMpicture'))))  # Find the 'Save CAM as picture' button 
        except:
            print("'Save CAM as picture' button could NOT be found!")  

        try:
            pictureButton.click()
        except Exception as error:
            print("there was an error clicking the 'Save CAM as picture' button for the following file: " + str(file) + " The error was" + str(error))


### Converts SVGs to PNGs, takes as parameters (1.) a dict with names and paths of SVG files (2.) the path of the folder where PNGs are saved
def convertSVGs(inputSVGsDict, pathOut):
    for fileName, filePath in inputSVGsDict.items():
        #print(fileName, " from ", filePath)
        try:
            fileOut = os.path.join(str(pathOut), str(str(fileName) + ".png"))
            cairosvg.svg2png(url=str(filePath), write_to=fileOut, dpi=1200, parent_height=1400, parent_width=2400, scale=3)
        except Exception as error: 
            print("There was an error converting ", (fileName), ": ", str(error))
        

def cropSVGs(inputSVGsDict, pathOut):
    for fileName, filePath in inputSVGsDict.items():
        #print(fileName, " from ", filePath)
        try:
            fileOut = os.path.join(str(pathOut), str(str(fileName) + ".png"))
            tree = etree.parse(open(filePath))
            toRemove = tree.xpath("/svg:svg/svg:rect[@id=\"background\"]",
                namespaces={"svg": "http://www.w3.org/2000/svg"})[0]
            parent = toRemove.getparent()
            parent.remove(toRemove)
            with open(fileOut, "wb") as out:
                out.write(etree.tostring(tree, pretty_print=True))
        except Exception as error: 
            print("There was an error cropping ", (fileName), ": ", str(error))