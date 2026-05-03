# Tag Diagnostics Plots

MoveApps

Github repository: *https://github.com/movestore/tag_diagnostics.git*

## Description
Creation of plots for diagnostics and monitoring of tags. Number of locations per day, battery voltage and fix rate, and any other attribute can be plotted

## Documentation
*Enter here a detailed description of your App. What is it intended to be used for. Which steps of analyses are performed and how. Please be explicit about any detail that is important for use and understanding of the App and its outcomes. You might also refer to the sections below.*

### Application scope
#### Generality of App usability
This App was developed for any taxonomic group. 

#### Required data properties
The App should work for any kind of (location) data.

### Input type
`move2::move2_loc`

### Output type
`move2::move2_loc`

### Artefacts
`tag_diagnostics_plots.pdf`: 

### Settings 




"Plot number of locations per day" T/F  plot_nb_lcs 
"Add battery voltage to previous plot" T/F add_vot
"Column name of battery voltage in the data set" string bat_attr
"Plot fix rate (approximation)" T/F plot_fix_rate
"Units to calculate fix rate" dropdown: "sec", "min", "hour","day"  unts_fix_rate
"Additional attributes to plot as lines" string attr_line
"Additional attributes to plot as a boxplot per day" string attr_boxplot
"Use local time to be displayed on the plot" T/F use_local_time
"Structure of pdf" - "Create pdf grouping plot per track" / "Create pdf grouping plots per attribute" pdfMode = c("perTrack", "perAttrib")




{
      "id": "attribs",
      "name": "Data attributes to plot",
      "description": "Provide the exact names of the data attributes that you want to plot (must be comma-separated! and without quotes). If unsure of attribute names or spelling, please run the first App in your workflow and check the 'event_attributes' in the 'App Output Details' (green 'i'). For definitions of Movebank attributes please refer to the Movebank Attribute Dictionary (https://www.movebank.org/cms/movebank-content/movebank-attribute-dictionary).",
      "defaultValue": null,
      "type": "STRING"
	}

### Changes in output data
The input data remains unchanged.

### Most common errors
misspelling attributes

### Null or error handling
