# CSS Tools for Wolfram Language

CSSTools is a package for importing [CSS](https://www.w3.org/Style/CSS/) files into the [Wolfram Language](https://www.wolfram.com/language/). CSSTools supports 11.0 and later versions of Wolfram Language deployments for the desktop, including [Wolfram Desktop](https://www.wolfram.com/desktop/) and [Mathematica](https://www.wolfram.com/mathematica/).

The initial release of CSSTools contains the core [CSS Level 2 Revision 1](https://www.w3.org/TR/CSS2/) specification. The tokenizer follows the [CSS Syntax Module Level 3](https://www.w3.org/TR/css-syntax-3/) specification and the selectors follow [Selectors Level 3](https://www.w3.org/TR/selectors-3/). Some additional modules are included such as [CSS Namespaces Module Level 3](https://www.w3.org/TR/css-namespaces-3/), [CSS Colors Module Level 4](https://www.w3.org/TR/css-color-4/), [CSS Media Queries Level 4](https://www.w3.org/TR/mediaqueries-4/), [CSS Custom Properties for Cascading Variables Module Level 1](https://drafts.csswg.org/css-variables/) and [CSS Values and Units Module Level 3](https://www.w3.org/TR/css-values-3/).

The Wolfram Desktop front end uses its own stylesheet system and translation from CSS to Wolfram Desktop is not one-to-one. Regardless, the goal is to conform with all included specifications as much as possible.  

### Installing the CSSTools release

The CSSTools release comes in the form of a `.paclet` file, which contains the entire package and its documentation. Download the latest release from the [Github repo's releases page](). To install, run the following command in the Wolfram Language:
```
PacletInstall["/full/path/to/CSSTools.paclet"]
```
This will permanently install the CSSTools paclet. The Wolfram Language will always use the latest installed version of CSSTools. Installed versions can be enumerated using the command:
```
PacletFind["CSSTools"]
```
And all versions can be uninstalled using the command:
```
PacletUninstall["CSSTools"]
```

### Using CSSTools

To access the documentation, open the notebook interface help viewer and search for CSSTools. The two results will be a guide page enumerating the most commonly used functions in CSSTools, and a detailed tutorial.

To start, load the CSSTools package and try importing some basic CSS rules.
```
Needs["CSSTools`"]
cssData = ImportString["h1 {font-size:14pt; color:red}", "CSS"]
```    
The result is a `Dataset` expression that matches a canonical layout of a CSS file with additional interpretations. Use the `CSSCascade` function to perform the CSS cascade algorithm and translate the imported CSS to Wolfram Desktop options.
```
CSSCascade[All (*all properties*), Cell (*as cell-level options*), cssData, All (*all rulesets*)]
```    
More details can be found in the included documentation.
    

### Where did this come from?

CSSTools is a paclet maintained by [Kevin Daily](https://github.com/KMDaily) of Wolfram Research. He began building CSSTools because he thought it would be neat to use the CSS stylesheet language in Wolfram Desktop. He did not realize how hard it would be to translate CSS to Wolfram Desktop options including the multitude of additional CSS modules that must all work together.

CSSTools is implemented completely in the Wolfram Language. It might have been better suited to build off of an existing library, but none were found that exactly matched the translations that were needed to match Wolfram Language code.

### ...and where's it going?

The hope is for CSSTools to be further developed by community support. Using the tokenizer and its related functions, additional properties should be straightforward to add to the package. Some CSS features that would be nice to see are vendor-specific properties added as recognizable properties. More ambitious additions would be animation rules.

### More...

See the following files for more information:

* [COPYING.md](COPYING.md) - CSSTools license
* [CONTRIBUTING.md](CONTRIBUTING.md) - Guidelines for contributing to CSSTools
* [ForDevelopers.md](ForDevelopers.md) - Walkthrough of how to contribute new features to CSSTools
* [HowToBuild.md](HowToBuild.md) - Instructions for building CSSTools as a paclet
