# Project Tools

A handful of repeated operations used elsewhere in this project's scripts are housed in this folder. See below for more information

## Script Explanations

- `fxn_....R` - Houses a single function. Documentation of function purpose / arguments is inside of the respective script. Script name corresponds to function name
- `flow_....R` - Houses a 'workflow' script. These scripts cannot be run by themselves and are instead 'sourced' in particular contexts. These scripts are not translated into functions because they create too many outputs and/or have too many "moving parts" for such a translation to be straightforward. Also ongoing maintenance of such functions can be extremely challenging.

