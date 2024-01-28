# ceh_xaringan

Minimalist `R` xaringan theme for html presentations using UKCEH style/colours.

## Example Title Slide

Here is the title slide:

![](img/ex/title.png)

## Full Example Presentation

Example presentation at [https://nerc-ceh.github.io/ceh_xaringan]( https://nerc-ceh.github.io/ceh_xaringan/). 

## Usage

just fork this and edit the front-matter YAML at the start of your Rmd file containing your presentation.


```
---
title: "your title"
subtitle: "subtitle"
author: "Your name"
date: "UKCEH </br> `r Sys.Date()`"
format:
  revealjs:
    theme: [default, ceh.scss]  
    slide-number: true
    chalkboard: 
      buttons: false
    preview-links: auto
    logo: img/logo/UKCEH-Logo_Short_Positive_RGB.png
---

```

## Rendering html from Rmarkdown
Run the command
`rmarkdown::render("./index.Rmd")`

or use the knit button in Rstudio.

## Offline Usage

* One big drawback is (used to be) that HTML presentations require a working internet connection during the presentation. 
* It's not always possible to guarantee that. Sigh.
* There is a great alternative/backup. Just print the HTML slides to PDF with [decktape](https://github.com/astefanutti/decktape)! I.e. you do

    ```
    decktape index.html presentation.pdf --chrome-arg=--disable-web-security
    ```
    to end up with this [PDF](presentation.pdf)

