## The bayesian Net to be used ####

## flow accummulation node: this will be a raster ####
f_acc_node <- cptable(~f_acc, values = c(0.2, 0.2, 0.2), levels = c("Low","Medium", "High"))

## slope node: this will be a raster ####
slope_node <- cptable(~slope, values = c(0.2, 0.2, 0.2, 0.2, 0.2), levels = c("Very steep", "Step", "Gentile", "Relatively flat", "Flat"))

## suitable topography: this will be a just probabilities (Not a raster) ####
suitable_topo_node_tmp <- make_gRain_CPT(parent_effects = list(c(2, 3, 1),
                                                               c(0, 0, 1, 2, 3)),
                                         parent_weights = c(3, 2),
                                         b = 2,
                                         child_prior = c(0.80, 0.20),
                                         child_states = c("Unsuitable", "Suitable"),
                                         parent_states = list(f_acc_node$levels,
                                                              slope_node$levels),
                                         parent_names = c('f_acc', 'slope'))
suitable_topo_node_values <- suitable_topo_node_tmp$values
suitable_topo_node_levels <- suitable_topo_node_tmp$levels
suitable_topo_node <- cptable (~suitable_topo|f_acc:slope, values = suitable_topo_node_values,levels = suitable_topo_node_levels)

## soil water node : this will be a raster ####
soil_water_content_node <- cptable(~soil_water_content, values = c(0.2, 0.2, 0.2, 0.2, 0.2), levels = c("Very low", "Low", "Medium", 'High', 'Very high'))

## wetness exposure: this will be a raster  ####
exposure2wetness_node <- cptable(~exposure2wetness, values = c(0.2, 0.2, 0.2, 0.2, 0.2), levels = c("Very low", "Low", "Medium", 'High', 'Very high'))

## suitable soils this will be a raster ####
suitable_soil_node_tmp <- make_gRain_CPT(parent_effects = list(c(0, 1, 2, 3, 0),
                                                               c(0, 1, 2, 3, 0)),
                                         parent_weights = c(3, 1),
                                         b = 5,
                                         child_prior = c(0.1, 0.3, 0.4, 0.3, 0.1),
                                         child_states = c("Unusable", "Unsuitable", "Moderately suitable", "suitable", "Highly suitable"),
                                         parent_states = list(soil_water_content_node$levels,
                                                              exposure2wetness_node$levels),
                                         parent_names = c('soil_water_content', 'exposure2wetness'))
suitable_soil_node_values <- suitable_soil_node_tmp$values
suitable_soil_node_levels <- suitable_soil_node_tmp$levels
suitable_soil_node <- cptable (~suitable_soil|soil_water_content:exposure2wetness, values = suitable_soil_node_values,levels = suitable_soil_node_levels)

## sensitivity to flood ####
sensitivity2flood_node <- cptable(~sensitivity2flood, values = c(0.2, 0.2, 0.2, 0.2, 0.2), levels = c("Very low", "Low", "Medium", 'High', 'Very high'))

## vegetation sensitibity to water ####
veg_sensitivity2water_var_node <- cptable(~veg_sensitivity2water_var, values = c(0.2, 0.2, 0.2, 0.2, 0.2), levels = c("Very low", "Low", "Medium", 'High', 'Very high'))

## characteristic vegetation ####
charact_veg_node_tmp <- make_gRain_CPT(parent_effects = list(c(1, 1, 2, 4, 5),
                                                             c(1, 1, 2, 4, 5)),
                                       parent_weights = c(1, 2),
                                       b = 5,
                                       child_prior = c(0.1, 0.3, 0.4, 0.3, 0.1),
                                       child_states = c('very dry', 'Dry', 'Moderately moist', 'Moist', 'Waterlogged'),
                                       parent_states = list(sensitivity2flood_node$levels,
                                                            veg_sensitivity2water_var_node$levels),
                                       parent_names = c('sensitivity2flood', 'veg_sensitivity2water_var'))
charact_veg_node_values <- charact_veg_node_tmp$values
charact_veg_node_levels <- charact_veg_node_tmp$levels
charact_veg_node <- cptable (~charact_veg |sensitivity2flood:veg_sensitivity2water_var, values = charact_veg_node_values,levels = charact_veg_node_levels)

## water present: this will be a raster ####
water_prznt_node_tmp <- make_gRain_CPT(parent_effects = list(c(1, 1, 2, 4, 5),
                                                             c(1, 2, 3, 4, 1),
                                                             c(1, 2, 3, 4, 1)),
                                       parent_weights = c(3, 2, 2),
                                       b = 3,
                                       child_prior = c(0.15, 0.35, 0.35, 0.15),
                                       child_states = c('Unexpected', 'Unsual', 'Commun', 'Permanent'),
                                       parent_states = list(sensitivity2flood_node$levels,
                                                            exposure2wetness_node$levels,
                                                            soil_water_content_node$levels),
                                       parent_names = c('sensitivity2flood', 'exposure2wetness', 'soil_water_content'))
water_prznt_node_values <- water_prznt_node_tmp$values
water_prznt_node_levels <- water_prznt_node_tmp$levels
water_prznt_node <- cptable (~water_prznt|sensitivity2flood:exposure2wetness:soil_water_content, values = water_prznt_node_values,levels = water_prznt_node_levels)

## temporal variation in vegetation indices: this will be a raster ####

## water present: this will be a raster ####
tempo_var_vi_node_tmp <- make_gRain_CPT(parent_effects = list(c(0, 1, 2, 4, 5),
                                                              c(0, 1, 2, 4, 0),
                                                              c(0, 1, 2, 4, 0)),
                                        parent_weights = c(3, 2, 3),
                                        b = 3,
                                        child_prior = c(0.05, 0.1, 0.35, 0.35, 0.05, 0.1),
                                        child_states = c('Extremely low', "Very low", "Low", 'High', 'Very high', 'Extremely high'),
                                        parent_states = list(sensitivity2flood_node$levels,
                                                             exposure2wetness_node$levels,
                                                             soil_water_content_node$levels),
                                        parent_names = c('sensitivity2flood', 'exposure2wetness', 'soil_water_content'))
tempo_var_vi_node_values <- tempo_var_vi_node_tmp$values
tempo_var_vi_node_levels <- tempo_var_vi_node_tmp$levels
tempo_var_vi_node <- cptable (~tempo_var_vi|sensitivity2flood:exposure2wetness:soil_water_content, values = tempo_var_vi_node_values,levels = tempo_var_vi_node_levels)

## flood at the beginning of the vegetation period: this will be a raster ####
flood_bgs_node <- cptable(~flood_bgs, values = c(0.2, 0.2, 0.2, 0.2), levels = c('Unexpected', 'Commun', 'Frequent', 'Permanent'))

## flooded at some points: this will be a just probabilities (Not a raster) ####
flooded_at_some_pts_node_tmp <- make_gRain_CPT(parent_effects = list(c(0, 0, 1, 3, 4),
                                                                     c(0, 1, 2, 3),
                                                                     c(0, 0, 1, 3, 4)),
                                               parent_weights = c(1, 2, 1),
                                               b = 3,
                                               child_prior = c(0.1, 0.3, 0.4, 0.3, 0.1),
                                               child_states = c("Very low", "Low", "Medium", 'High', 'Very high'),
                                               parent_states = list(suitable_soil_node$levels,
                                                                    water_prznt_node$levels,
                                                                    charact_veg_node$levels),
                                               parent_names = c('suitable_soil', 'water_prznt', 'charact_veg'))
flooded_at_some_pts_node_values <- flooded_at_some_pts_node_tmp$values
flooded_at_some_pts_node_levels <- flooded_at_some_pts_node_tmp$levels
flooded_at_some_pts_node <- cptable (~flooded_at_some_pts|suitable_soil:water_prznt:charact_veg, values = flooded_at_some_pts_node_values,levels = flooded_at_some_pts_node_levels)

## Vegetation variation caused by flood: this will be a just probabilities (Not a raster) ####
veg_var_caused_by_flood_node_tmp <- make_gRain_CPT(parent_effects = list(c(1, 2, 3, 4, 5, 6),
                                                                         c(0, 1, 2, 3, 0),
                                                                         c(2, 3, 1),
                                                                         c(0, 1, 2, 0)),
                                                   parent_weights = c(2, 1, 2, 1),
                                                   b = 3,
                                                   child_prior = c(0.1, 0.3, 0.4, 0.3, 0.1),
                                                   child_states = c('Very low', 'Low', 'Medium', "High", 'Very high'),
                                                   parent_states = list(tempo_var_vi_node$levels, 
                                                                        flood_bgs_node$levels,
                                                                        f_acc_node$levels,
                                                                        water_prznt_node$levels),
                                                   parent_names = c('tempo_var_vi', 'flood_bgs', 'f_acc', 'water_prznt'))
veg_var_caused_by_flood_node_values <- veg_var_caused_by_flood_node_tmp$values
veg_var_caused_by_flood_node_levels <- veg_var_caused_by_flood_node_tmp$levels
veg_var_caused_by_flood_node <- cptable (~veg_var_caused_by_flood|tempo_var_vi:flood_bgs:f_acc:water_prznt, values = veg_var_caused_by_flood_node_values,levels = veg_var_caused_by_flood_node_levels)

## FBFS/FBS: this is the target node and this will be a just probabilities (Not a raster)####
p_tools_node <- cptable(~p_tools, values = c(0.20, 0.80), levels = c("Low", 'High'))

pixel_is_FBFS_node_tmp <- make_gRain_CPT(parent_effects = list(c(0, 1),
                                                               c(0, 0, 1, 3, 4),
                                                               c(0, 0, 1, 3, 4),
                                                               c(0, 1)),
                                         parent_weights = c(3, 1, 2, 3),
                                         b = 4,
                                         child_prior = c(0.05, 0.4, 0.6, 0.4, 0.05),
                                         child_states = c('Highly unlikely', 'Unlikely', 'Possible', "Likely", "Highly likely"),
                                         parent_states = list(suitable_topo_node$levels,
                                                              flooded_at_some_pts_node$levels,
                                                              veg_var_caused_by_flood_node$levels,
                                                              p_tools_node$levels),
                                         parent_names = c('suitable_topo', 'flooded_at_some_pts', 'veg_var_caused_by_flood', 'p_tools'))
pixel_is_FBFS_node_values <- pixel_is_FBFS_node_tmp$values
pixel_is_FBFS_node_levels <- pixel_is_FBFS_node_tmp$levels
pixel_is_FBFS_node <- cptable (~pixel_is_FBFS|suitable_topo:flooded_at_some_pts:veg_var_caused_by_flood:p_tools, values = pixel_is_FBFS_node_values,levels = pixel_is_FBFS_node_levels)

## compiling the Bayesian network ####

net <- compileCPT(list(f_acc_node, slope_node,suitable_topo_node, soil_water_content_node,exposure2wetness_node, suitable_soil_node, sensitivity2flood_node, 
                       veg_sensitivity2water_var_node, charact_veg_node, p_tools_node, water_prznt_node, tempo_var_vi_node, flood_bgs_node, flooded_at_some_pts_node, veg_var_caused_by_flood_node,pixel_is_FBFS_node))

## making the Bayesian network ####
# gRain bn
network <- grain(net)
# bnlearn bn
network_bn_fit <- as.bn.fit(network)

