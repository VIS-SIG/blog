Longitudinal data – an animation

David Carr

Often enough, new visualization ideas come from external sources – at a recent BBS seminar, Stephen Ruberg 
(Advanced Analytics, Eli Lilly) presented a novel way to display longitudinal data. 

The X axis ticks corresponds to individual patients (the order by baseline Pain value), while the Y axis 
is the Change from Baseline of Pain value at various time points. The weekly measured data are linearly 
interpolated (in this case 6-hourly), and are shown in RED dots, while the threshold of at least a 30 point 
reduction in Pain is shown in light blue shading. This was programmed in SAS, using the SVG device.

