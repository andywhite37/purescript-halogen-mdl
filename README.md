# Purescript Halogen MDL (Material Design Lite)

[Live demo](https://rawgit.com/andywhite37/purescript-halogen-mdl/master/examples/dist/index.html)

## Overview

`purescript-halogen-mdl` is a [Purescript Halogen](https://github.com/slamdata/purescript-halogen) library to
componentize the [Google Material Design Lite (MDL)](https://getmdl.io/index.html)
UI components, classes, attributes, and native javascript effects so that
they can be used within a Halogen application.

Note: [MDL](https://getmdl.io/index.html) is now a "deprecated" library which is in the process of being
replaced with a new UI library called [Material Components Web](https://github.com/material-components/material-components-web).
Material Components Web can be thought of as version 2 of MDL, but is not backwards compatible.

## Documentation

The example application is currently the best way to see how to use the components from Halogen.

Each MDL component is wrapped up in a Purescript module.  Some modules just contain type-safe constants
for classes and attributes.  Some contain helper functions for creating commonly-used blocks of HTML, and
some Purescript modules contain actual Halogen components which attempt to wrap up all the native behavior.

The main native MDL function `upgradeElement` is exposed in several different ways as a native import in the `MDL` module.
This function is used to "upgrade" the DOM elements to give them the custom MDL behaviors, like button ripples, animations, etc.

## Setup

```
> npm install
> bower install
```

## Examples

```
# In a terminal:
# Run compiler watch task:
> npm run watch:examples

# In another terminal:
# Run static http file server from this location (e.g. node-static, live-server, etc.)
> cd examples/dist
> live-server .
or
> static .
etc.
```
