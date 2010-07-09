import os
import sys
from launchpadlib.launchpad import Launchpad, STAGING_SERVICE_ROOT
from launchpadlib.launchpad import EDGE_SERVICE_ROOT
import glob
import yaml

APP_NAME = 'bombardier'
CACHE_DIR = os.path.expanduser('~/.launchpadlib/cache')
LAUNCHPAD_ENVIRONMENT = EDGE_SERVICE_ROOT

def base(filename):
    "Return the name of the file without directory info"
    base_name = filename.split(os.path.sep)[-1]
    return base_name

def upload_file(environment):
    "Send a file to Launchpad"
    filenames = glob.glob("dist/*.tar.gz")
    if len(filenames) > 1:
        print "Directory is unclean"
        sys.exit(1)
    
    filename = filenames[0]
    file_content = open(os.path.join(filename), 'r').read()

    signature_filename = filename+".asc"
    if not os.path.isfile(signature_filename):
        os.system("gpg --armor --sign --detach-sig %s" % filename)

    signature_content = open(signature_filename, 'r').read()

    component_info = yaml.load(open("component_info.yml").read())
    description = component_info["description"]

    print("Logging in to Launchpad %s..." % LAUNCHPAD_ENVIRONMENT)
    launchpad = Launchpad.login_with(APP_NAME, environment, CACHE_DIR)
    project = launchpad.projects["bombardier"]
    series1 = project.series[1] # 1.00
    alpha = series1.releases[0]
    #beta = series1.releases[1]

    print("Uploading file (%s)..." % description)
    alpha.add_file(file_type="Code Release Tarball",
                   description = description,
                   filename = base(filename),
                   content_type = "application/x-gtar",
                   file_content = file_content,
                   signature_filename = base(signature_filename),
                   signature_content = signature_content,
                  )

def main():
    "This is all we're going to do"
    environment = EDGE_SERVICE_ROOT
    if len(sys.argv) > 1:
        if sys.argv[1].lower().startswith('--s'):
            environment = STAGING_SERVICE_ROOT
    upload_file(environment)

if __name__ == "__main__":
    main()
