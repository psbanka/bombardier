#!/cygdrive/c/Python23/python.exe

import re, string

TEMPLATE     = "template.html"
FOOTER       = "footer.html"

def mainMenu(menuList):
    output = ""
    menuHtml = '        <a href="/%(link)s" title="%(explanation)s">%(name)s</a> |'
    for menuItem in menuList:
        explanation = menuItem.get('explanation')
        name = menuItem['name']
        link = name
        if menuItem.has_key("link"):
            link = menuItem["link"]
        output += menuHtml % ({'name': name, 'link':link, 'explanation':explanation})
    return output

def subMenu(menuList):
    output = ""
    menuHtml = '      <li><a href="%(link)s" title="%(explanation)s">%(name)s</a></li>'
    for menuItem in menuList:
        explanation = menuItem.get('explanation')
        name = menuItem['name']
        link = name
        if menuItem.has_key("link"):
            link = menuItem["link"]
        output += menuHtml % ({'name': name, 'link':link, 'explanation':explanation})
    return output

def substituteFile(fileName, inputData):
    varMatch = re.compile("\%\((.*?)\)s")
    configData = open(fileName, 'r').read()
    variables = varMatch.findall(configData)
    output = []
    for line in configData.split('\n'):
        variables = varMatch.findall(line)
        configDict = {}
        for variable in variables:
            configValue = inputData[variable]
            configDict[variable] = configValue
        if configDict == {}:
            output.append(line)
        else:
            output.append(line % configDict)
    outputData = string.join(output, '\n')
    return outputData

def generateHtml(title="Bombardier Server",
                 heading="Bombardier",
                 subtitle="Open-source Windows Package Management",
                 mainMenuList = {},
                 subMenuList = [],
                 body='',
                 footer='',
                 meta=''):
    inputData = {"meta": meta,
                 "title": title,
                 "heading":heading,
                 "subtitle":subtitle,
                 "menu":mainMenu(mainMenuList),
                 "submenu":subMenu(subMenuList),
                 "body":body}
    outputData  = substituteFile(TEMPLATE, inputData)
    outputData += substituteFile(FOOTER, {"footer":footer})
    return outputData
                               
def generateNoFooter(title="Bombardier Server",
                     heading="Bombardier",
                     subtitle="Test-driven networking",
                     mainMenuList = {},
                     subMenuList = [{"name":"What's new"},{"name":"Who"}],
                     body='',
                     footer='',
                     meta=''):
    inputData = {"meta": meta,
                 "title": title,
                 "heading":heading,
                 "subtitle":subtitle,
                 "menu":mainMenu(mainMenuList),
                 "submenu":subMenu(subMenuList),
                 "body":body}
    return substituteFile(TEMPLATE, inputData)
                     
