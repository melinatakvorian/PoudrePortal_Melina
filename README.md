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
  Getting an error somewhere in the map building process, I think before addLayersControl