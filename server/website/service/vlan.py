import string, StringIO, webUtil

from macaddr import CiscoBeast
from static import *

"""This thing allows queries and puts to the VLAN configuration system,
allowing a system to change its VLAN on the fly."""

def get(request, logger, errlog):
    macAddress = request.args.get("macaddress")
    if not macAddress:
        macAddress = ''
    else:
        macAddress = macAddress[0]
    logger.info( "VLAN Service obtained request to view %s " % macAddress)
    cb = CiscoBeast(SWITCH_ADDRESS, SWITCH_LOGINPASS, SWITCH_ENABLEPASS)
    cb.login()
    output = []
    if macAddress:
        output.append("%s\n" % cb.get_vlan(macAddress))
    else:
        cb.get_mac_addr_table()
        for vlan, mac, port in cb.macAddrTable:
            output.append("%s,%s,%s" % (vlan, mac, port))
    return string.join(output, '\n')

def put(request, logger, errlog):
    macAddress = request.args.get("macaddress")
    if not macAddress:
        errlog.info( "VLAN Service received a PUT with no macaddress" )
        return webUtil.err400(request, errlog, "Bad put request", "Provide the macaddress\n")
    macAddress = macAddress[0]
    vlan = request.content.read().strip()
    if not vlan:
        errlog.info( "VLAN Service received a PUT with no vlan data" )
        return webUtil.err400(request, errlog, "Bad put request", "Provide the vlan as part of the PUT\n")
    request.write("Setting vlan %s for mac address %s\n" % (vlan, macAddress))
    cb = CiscoBeast(SWITCH_ADDRESS, SWITCH_LOGINPASS, SWITCH_ENABLEPASS)
    cb.login()
    currentVlan = cb.get_vlan(macAddress)
    proceed = False
    if currentVlan == "":
        request.write("Could not look up current VLAN\n")
        request.finish()
    elif currentVlan == vlan:
        request.write("Already set.\n")
        request.finish()
    else:
        request.write("Setting new VLAN from %s to %s\n" % ( currentVlan, vlan ))
        request.finish()
        cb.set_vlan(macAddress, vlan)
    return
