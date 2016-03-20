
### Disclaimers

1. The City of Seattle would like all users of their data products to know that:
    
    > The data made available here has been modified for use from its original source, which is the City of Seattle. Neither the City of Seattle nor the Office of the Chief Technology Officer (OCTO) makes any claims as to the completeness, timeliness, accuracy or content of any data contained in this application; makes any representation of any kind, including, but not limited to, warranty of the accuracy or fitness for a particular use; nor are any such warranties to be implied or inferred with respect to the information or data furnished herein. The data is subject to change as modifications and updates are complete. It is understood that the information contained in the web feed is being used at one's own risk.

2. The purpose of this tool is to *approximate* Seattle's urban villages using tract and block group geometries from the US Census. **The associations created by this tool are not official designations and are for research purposes only**. 

### Notes

The associations are the result of the following procedure:

1. Begin with all Seattle census blocks[^1] and their respective urban village designations [@844]
2. Select which census demographic value to use (e.g., housing units [@845], population [@846])
3. Group the blocks by census geometry, then sum the census demographic and group by urban village
4. The urban village with the highest sum is passed on to the census geometry
5. For urban villages that are excluded from the list (because they didn't have the highest sum for any census geometry), remove 'Outside Villages' from the sum list and include the urban village with the new highest sum[^2] 

### Resources


[^1]: All polygons are clipped to remove waterbodies [@847]
[^2]: Only one census unit (e.g., tract, block group) per excluded village
