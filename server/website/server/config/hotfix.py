import template, string, webUtil
from twisted.web import server
from static import *

def watchPkgCreation(args, errlog):
    request, url, kbNumber, appliesTo = args
    args={"packagename":kbNumber, "type":"hotfix"}
    if appliesTo:
        args["info"] = appliesTo
    request.write("<ul>")
    fileHandler = webUtil.servicePut("package", args=args, data=url, live=True)
    data = fileHandler.readline()
    while data:
        request.write("<li>%s" % data)
        data = fileHandler.readline()
    request.write('</li>')
    return ["<p><strong>finished</strong></p>"]

def post(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    kbNumber = request.args.get("kbnumber")
    if kbNumber:
        kbNumber = kbNumber[0]
    else:
        return webUtil.err400(request, errlog, "Missing data", "need kbnumber field\n")
    url  = request.args.get("url")
    if url:
        url = url[0]
    else:
        return webUtil.err400(request, errlog, "Missing data", "need url field\b")
    appliesTo = request.args.get("appliesto")
    if appliesTo:
        appliesTo = appliesTo[0]
    else:
        return webUtil.err400(request, errlog, "Missing data", "need appliesto field\b")
##     output = [HEADER]
    page = template.generateNoFooter(mainMenuList=website.server.list,
                                     subMenuList=subMenuList,
                                     body='<h1>Creating Package Hotfix-%s.spkg&hellip;</h1>' % kbNumber)
    request.write(page)
    pThread = webUtil.Process(request=request,
                              function=watchPkgCreation,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=[request, url, kbNumber, appliesTo],
                              capOnly=True)
    pThread.start()
    return server.NOT_DONE_YET

def hotfixCreationForm(args, errlog):
    output = []
    packageNames = webUtil.serviceRequest("packages")
    output.append('<h1>Create a new Hotfix package</h1>')
    output.append('<form action="./hotfix" method=POST>')
    output.append('<table>')
    output.append('<tr><td>Input the Microsoft Knowledge Base ID:</td>')
    output.append('<td><INPUT TYPE="text" NAME="kbnumber" maxlength="30" size="44"></INPUT></td></tr>')
    output.append('<tr><td>Input the URL to obtain the hotfix:</td>')
    output.append('<td><INPUT TYPE="text" NAME="url" maxlength="250" size="44"></INPUT></td></tr>')
    output.append('<tr><td>What does this patch?</td>')
    output.append('<td><SELECT NAME="appliesto"> <OPTION SELECTED>WINDOWS 2000 - ANY')
    output.append('<OPTION>WINDOWS 2000 - SERVER')
    output.append('<OPTION>WINDOWS 2000 - PROFESSIONAL')
    output.append('<OPTION>WINDOWS XP')
    for packageName in packageNames:
        output.append('<OPTION>%s' % packageName)
    output.append('</SELECT></td></tr>')        
    output.append('</table><hr>')
    output.append('''<input type="submit" value="SUBMIT">\n </form></body>''')
    return output
    

def get(request, logger, errlog):
    path = __name__.split('.')
    exec("import %s" % string.join(path[:-1],'.'))
    exec("subMenuList = %s.list" % string.join(path[:-1],'.'))
    pThread = webUtil.Process(request=request,
                              function=hotfixCreationForm,
                              mainMenuList = website.server.list,
                              subMenuList = subMenuList,
                              args=[])
    pThread.start()
    return server.NOT_DONE_YET
