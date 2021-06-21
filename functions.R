## sets of functions

wrangle = function(df){
  
  df = df %>% separate(X7.3.Zip, c('lat', 'long'), sep = '\\|') %>% 
    mutate( long = as.double(long), lat = as.double(lat), 
            Longitude = as.numeric(long), Latitude = as.numeric(lat) )
  
  
  names(df) = names(df) %>% 
    str_replace('linking','linking.with.organizations.and.institutions' ) %>% 
    str_replace('bridging','connecting.with.other.people' ) %>% 
    str_replace('bonding','reinforcing relationships with trusted friends and neighbors' ) %>% str_replace('X2.1.The.experience.I.shared.describes.the.impact.this.has.had.in.relation.to....', 'X2.1.Impact.the.event.had.on.') %>% str_replace('X7.1.The.impact.of.Event.on.home.', 'X7.1.The.impact.the.event.had.on.home.')
  
  df =  df  %>% 
    mutate(storyTime = 
             case_when(X1.3.Story.relates.to_before.the.event..hurricane.and.or.earthquakes. == 
                         1 ~ 'Before hurricane or earthquakes',
                       X1.3.Story.relates.to_during.the.event == 
                         1 ~ 'During the event',
                       X1.3.Story.relates.to_immediately.after == 
                         1 ~ 'Immediately after',
                       X1.3.Story.relates.to_longer.term.relief.and.recovery == 
                         1 ~'Longer term relief and recovery ',
                       X1.3.Story.relates.to_all.of.the.above == 1 ~ 'All', 
                       X1.3.Story.relates.to_NA == 1 ~ 'NA'
             ))
  
  
  return(df)

}


getPoints = function(df){
  
  sm = st_as_sf(x = df, coords = c('long', 'lat'), crs = 4326)
  return(sm)
}

bindGeo = function(sm, geo){
  
  
  s = sm %>% st_intersection(geo) %>% st_drop_geometry()
  return(s)
  
}

