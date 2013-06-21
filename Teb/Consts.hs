module Teb.Consts where

version = "1.2.2"
versionDescription = "Teb repository manager (version " ++ version ++ ") (h)"
coreDirectory = ".teb"
manifestLocationFilename = "tebmanifestlocator.xml"
manifestFilename = "teb.xml"
usageStrings = [
	     versionDescription,
	     "Syntax:",
	     "\tteb COMMAND",
	     "\t  init TEB_REPO_LOCATION [MANIFEST_FILENAME]",
	     "\t    This command initialises a teb repo in the current working directory using the arguments passed on the commandline",
	     "\t    TEB_REPO_LOCATION - the remote location of a teb repository",
	     "\t    MANIFEST_FILENAME - the name of the teb manifest file to use within the teb repository [default : " ++ manifestFilename  ++ "]", "",
	     "\t  status - outputs the results of a git status command for each git repository specified in the teb repository", "",
	     "\t  fetch [PROJECT_STATUS] - executes a git fetch command for each git repository with status PROJECT_STATUS specified in the teb repository",
	     "\t    PROJECT_STATUS - allowed values are 'active' and 'inactive', if no PROJECT_STATUS is specified the default is 'inactive'", "",
	     "\t  sync - executes a git rebase origin/BRANCH command for each inactive git repository specified in the teb repository", "",
	     "\t  info - output the teb information for each repository specified in the teb repository", "",
	     "\t  reinit - fetch and rebase the teb manifest repo and clone any new projects", "",
	     "\t  version - show version information for teb", ""]