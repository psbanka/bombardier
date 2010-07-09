import os
import sys
from launchpadlib.launchpad import Launchpad, STAGING_SERVICE_ROOT
from launchpadlib.launchpad import EDGE_SERVICE_ROOT
import glob

APP_NAME = 'bombardier'
CACHE_DIR = os.path.expanduser('~/.launchpadlib/cache')

def base(file_name):
    "Return the name of the file without directory info"
    base_name = file_name.split(os.path.sep)[-1]
    return base_name

def upload_file(environment):
    "Send a file to Launchpad"
    launchpad = Launchpad.login_with(APP_NAME, environment, CACHE_DIR)
    project = launchpad.projects["bombardier"]
    series1 = project.series[1] # 1.00
    alpha = series1.releases[0]
    beta = series1.releases[1]

    file_names = glob.glob("dist/*.tar.gz")
    if len(file_names) > 1:
        print "Directory is unclean"
        sys.exit(1)
    
    file_name = file_names[0]
    file_content = open(os.path.join(file_name), 'r').read()

    signature_file_name = file_name+".asc"
    os.system("gpg --armor --sign --detach-sig %s" % file_name)
    signature_content = open(signature_file_name, 'r').read()

    alpha.add_file(file_type="Code Release Tarball",
                   description = "Bombardier file",
                   file_name = base(file_name),
                   content_type="application/x-gtar",
                   file_content=file_content,
                   signature_filename = base(signature_file_name),
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
