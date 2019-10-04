# Building CSSTools Components

The CSSTools repo has two buildable components: the documentation and then the distributable paclet.

## How to build CSSTools documentation

1. [Install Eclipse and the Wolfram Workbench plug-in for Eclipse](https://support.wolfram.com/kb/27221)
2. Import the CSSTools project into Eclipse
3. Open the "Application Tools" under the menu Window >  Show View > Application Tools
4. In the Application Tools window select the CSSTools project
5. Click "Build" in the documentation panel


## How to build the CSSTools paclet
1. If making any changes to the project, update the PacletInfo.m file's Version. 
    If a bugfix, then add one to the 3rd decimal e.g. 1.0.1 --> 1.0.2.
    If a feature, then add one to the 2nd decimal e.g. 1.0.0 --> 1.1.0
2. In Eclipse's Application Tools window, select the CSSTools project
3. Click "Deploy Application"
4. In the new dialog choose a folder to deploy the project to. 
5. Select all the files from the project. The "Old" and "Testing" directories can be excluded from the deployment.
6. Click "Next" then "Finish" to close the dialog.
7. In Mathematica, run `PackPaclet[/path/to/deployed/application]` where the path is the location chosen in the previous step.
8. The newly generated paclet will be in the parent location of the path in the previous step.