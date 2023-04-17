# PoudrePortal_Melina

4/12 Right now I am working on code integration
  trying to slowly copy Caitlin's structure for my reactive objects and goals
  
  I think I updated everything in ui and server. I want to make the outline
  better though (indentations correct)
  
4/13 Running Caitlin's code
  GENERALLY: everything is stacked onto the left side instead of being two panels of the page
  I ran her code, the Cameron Peak layer caused the map to not appear
  I commented it out and the map doesn't throw an error but nothing is produced
    I am going to continue commenting unnecessary things out until something is produced
    starting to think this relates to the error with the object below
  The Table tab says "Error: object 'Site' not found"
    I need to trace that down because I bet i just used the wrong reference name
    changed to 'sites' and it didn't work, same error and no map
    
    FOUND IT!? I think I need to create that table with the unique site info and then load it in to be able to refer to 'sites'
    sites <- readRDS("data/sites_table.RDS")
    I realized that she renamed her other main table, so I have renamed that to chem_data and went through and corrected it
    
4/14 in the am
  finished creating the official sites_table and chem_data RDS's. they look good and have been working smoothly
  Getting an error somewhere in the map building process, I think before addLayersControl
  
  Error in Map: no applicable method for 'metaData' applied to an object of class "character"
  Error in Table: 'data' must be 2-dimensional (e.g. data frame or matrix) --- I thought it was but it appears to save as a value not df, how to fix?

4/15 in the pm
The page is loading with a blank map, the columns are correct, the Table Tab looks like it works but throws the second error (below) when you try to click something

  Warning: Error in col2rgb: invalid color name 'Partially Burned'
  Warning: Error in vec_rbind: `c()` method returned a vector of unexpected size 44 instead of 42.
ℹ In file c.c at line 412.
ℹ Install the winch package to get additional debugging info the next time you get this error.
ℹ This is an internal error that was detected in the vctrs package.
  Please report it at <https://github.com/r-lib/vctrs/issues> with a reprex and the full backtrace.
  
  PAGE LOADED WITH A MAP. But it was zoomed out to the whole world and the points were not plotted
  The Table tab greyed out as soon as I selected a site and threw the same error as before
  I just noticed that the start and end dates are not correct... some start dates are after the end dates fixed it!
  
  Went through and corrected some smaller things, still getting no points on the map and a table with the same errors as before
  
4/17 in the am/pm
  I tried to map the points with tmap, and they were successfully plotted by the reactivity was not working correctly.
  
  The map sites show up!!! But the selections don't do anything, and clicking on the site doesn't work
  I just realized that the selections for the map will not work because they contradict each other