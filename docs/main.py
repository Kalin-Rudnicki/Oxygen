import os
import re

def define_env(env):

    oxygen_version_string = ''

    """
    Start by attempting to read git describe --tags
    """
    if not oxygen_version_string:
        try:
            pipe = os.popen('git describe --tags --abbrev=0')
            oxygen_version_string = pipe.read().replace("\n", '').strip()
            pipe.close()
        except Exception:
            oxygen_version_string = ''

    my_v1 = oxygen_version_string

    """
    If that fails then fallback on CI_LATEST_TAG env var we passed manually to Docker image 
    """
    if not oxygen_version_string:
        try:
            oxygen_version_string = env.conf['extra']['local']['tag']
        except KeyError:
            oxygen_version_string = ''

    my_v2 = oxygen_version_string

    """
    Finally fallback on something :/
    """
    if not oxygen_version_string:
        oxygen_version_string = 'oxygen_version'

    my_v3 = oxygen_version_string


    """
    If git describe tells us that this is NOT a git tag but git tag + some offset, we need to add -SNAPSHOT to match sbt 
    """
    if re.compile('.+-[0-9]+-g[0-9a-z]{8}').match(oxygen_version_string):
        oxygen_version_string = oxygen_version_string[0:-1] + '-SNAPSHOT'
    elif re.compile('.+-[0-9]+-[0-9a-z]{8}').match(oxygen_version_string):
        oxygen_version_string = oxygen_version_string + '-SNAPSHOT'

    my_v4 = oxygen_version_string

    print(f"[DEBUGGING-STUFF] v1={myV1}, v2={my_v2}, v3={my_v3}, v4={my_v4}")

    @env.macro
    def oxygen_version():
        return oxygen_version_string
