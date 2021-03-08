Longitudinal data – an animation

David Carr

Often enough, new visualization ideas come from external sources – at a recent BBS seminar, Stephen Ruberg 
(Advanced Analytics, Eli Lilly) presented a novel way to display longitudinal data. 

The X axis ticks corresponds to individual patients (the order by baseline Pain value), while the Y axis 
is the Pain value at various time points. The weekly measured data are linearly interpolated (in this case 
6-hourly), and are shown in RED dots. This was programmed in SAS, using the SVG device.

