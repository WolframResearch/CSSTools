# Building CSSTools Components

The CSSTools repo has two buildable components: the documentation and then the distributable paclet.

## How to build CSSTools documentation

1. [Install Eclipse and the Wolfram Workbench plug-in for Eclipse](https://support.wolfram.com/kb/27221)
2. Import the CSSTools project into Eclipse
3. Open the "Application Tools" under the menu Window >  Show View > Application Tools
4. In the Application Tools window select the CSSTools project
5. Click "Build" in the documentation panel


## How to build the CSSTools paclet
The paclet structure is self-contained within the project structure. The paclet is the subdirectory "CSSTools". Assuming all changes to the package files and documentation are complete:

1. Update the PacletInfo.m file's Version. 
    If a bugfix, then add one to the 3rd decimal e.g. 1.0.1 --> 1.0.2.
    If a feature, then add one to the 2nd decimal e.g. 1.0.0 --> 1.1.0
2. In Mathematica 12.1 or newer run `CreatePacletArchive["<localRepo>/CSSTools"]`, else use `PackPaclet` instead.
3. The newly generated paclet will be in the parent location of the path in the previous step.