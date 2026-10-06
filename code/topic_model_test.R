library(tidyverse)

dat <- tibble::tribble(
    ~statement, 
"Regular cycling strengthens cardiovascular function by steadily challenging the heart and lungs, promoting improved circulation and oxygen flow. Over time, this consistent aerobic exercise reduces the risk of heart disease, enhances endurance, and fosters long-term physical vitality, making it an accessible and sustainable way to support lifelong cardiovascular health",
  
  "Cycling boosts respiratory efficiency by encouraging deeper, more controlled breathing patterns during sustained effort. This helps expand lung capacity, improve oxygen exchange, and strengthen respiratory muscles. As a low-impact activity, cycling provides these benefits without placing excessive strain on the joints, making it suitable for people of various fitness levels",
  
  "Consistent bike riding supports weight management by increasing daily caloric expenditure through moderate to vigorous physical activity. The rhythmic motion of pedaling engages large muscle groups, promoting fat loss and muscle toning. Combined with enjoyable outdoor movement, cycling helps individuals maintain a healthy body composition without requiring lengthy or overly intense workouts",
  
  "Engaging in regular cycling routines strengthens lower-body musculature, particularly the quadriceps, hamstrings, calves, and glutes. These muscle groups become more efficient over time, improving overall mobility and stability. The continuous, circular movement pattern also enhances joint lubrication, supporting long-term joint health while reducing the risk of injury common in high-impact exercises",
  
  "Cycling contributes significantly to mental well-being by stimulating endorphin release and reducing cortisol levels. This natural stress relief helps combat anxiety and tension. The combination of rhythmic motion, outdoor scenery, and a sense of personal accomplishment makes cycling an effective and enjoyable way to support emotional resilience and mental clarity",
  
  "Frequent cycling helps regulate blood sugar by increasing the body’s sensitivity to insulin and promoting efficient glucose utilization within the muscles. These effects support metabolic health, particularly for individuals managing prediabetes or type 2 diabetes. Regular riding encourages better energy stability throughout the day and reduces long-term metabolic risks",
  
  "Routine cycling provides a low-impact exercise option that supports joint mobility and reduces stiffness. Because biking minimizes pressure on knees, hips, and ankles, it allows individuals to strengthen supporting muscles without aggravating joint issues. This makes cycling especially valuable for aging adults or those recovering from high-impact injuries",
  
  "Consistent cycling improves overall balance, agility, and coordination by requiring controlled body positioning and smooth pedal strokes. Over time, these neuromuscular benefits enhance stability during daily activities, reducing the likelihood of falls. This makes cycling not only an enjoyable pastime but also a functional exercise that supports safer movement patterns",
  
  "Cycling helps sharpen cognitive performance by increasing blood flow to the brain, supporting memory, focus, and problem-solving abilities. The rhythmic nature of riding can also facilitate creative thinking and mental reset. When performed outdoors, exposure to fresh air and varied environments enhances these cognitive benefits even further",
  
  "Regular cycling contributes to long-term disease prevention by lowering inflammation, improving circulation, and supporting healthier metabolic function. These combined effects help reduce the risk of conditions such as hypertension, stroke, and certain cancers. As a flexible, adaptable activity, cycling promotes consistent exercise habits that are easier to sustain over time",
  
  "Riding a bike for daily commuting often shortens travel times by allowing cyclists to bypass congested streets and stalled intersections. Unlike cars trapped in traffic, riders maintain steady movement, leading to more predictable and efficient arrival times. This makes cycling a practical solution for navigating busy urban environments",
  
  "Cyclists frequently avoid delays caused by traffic jams because bikes can maneuver through narrow lanes and designated cycling paths. This flexibility dramatically reduces time wasted in standstill traffic. As cities expand their cycling infrastructure, commuters increasingly experience smoother, faster routes than many individuals traveling by car or bus",
  
  "Using a bike for commuting reduces dependency on unpredictable public transportation schedules. Riders no longer wait for delayed buses or trains, allowing them to control their departure times with precision. This autonomy enhances commute reliability and often results in shorter, more efficient trips, particularly during peak urban congestion",
  
  "Bike commuting frequently results in shorter door-to-door travel because riders are not required to locate parking spaces or navigate large parking facilities. By traveling directly to their destination, cyclists eliminate hidden time costs that drivers regularly face. This directness makes biking an appealing option for time-conscious commuters",
  
  "Urban cyclists often shorten their commute by taking advantage of bike-only shortcuts, pedestrian pathways, and routes inaccessible to cars. These alternative pathways create more streamlined travel patterns, reducing total distance and time spent commuting. This advantage is particularly noticeable in dense city centers where vehicle flow is heavily restricted",
  
  "Because bicycles accelerate quickly and maintain efficient speeds in stop-and-go environments, riders frequently outpace cars in urban traffic. At intersections, cyclists lose less time due to shorter waiting cycles and reduced congestion. This consistent forward movement leads to faster and more predictable commuting outcomes throughout the workweek",
  
  "In many metropolitan areas, bike lanes provide uninterrupted travel corridors that shield cyclists from car traffic delays. These dedicated pathways allow steady movement even during peak commute hours. As a result, many bike commuters experience significantly shorter overall travel times compared to drivers navigating crowded multi-lane road systems",
  
  "Cycling eliminates time spent refueling, parking, or sitting at drive-throughs, which collectively slow car commutes. Riders travel more directly, with fewer interruptions, leading to a faster overall journey. This simplicity helps commuters incorporate cycling into their routine without the logistical barriers that commonly extend automobile travel times",
  
  "Bikes provide efficient point-to-point travel by allowing commuters to follow the most direct route between home and work, free from detours imposed by one-way roads or traffic restrictions. This streamlined navigation reduces unnecessary travel distance and often results in significantly faster commute times than traditional motor-vehicle routes",
  
  "Commuting by bike often avoids the cascading delays triggered by accidents, construction, or road closures that commonly disrupt car travel. Cyclists can detour easily around obstacles and continue moving without major slowdowns. This adaptability significantly enhances the overall speed and reliability of travel within crowded urban areas",
  
  "Group cycling fosters a sense of community by bringing riders together for shared goals, routes, and experiences. These social interactions encourage continued participation, create supportive networks, and transform fitness into a collaborative activity. As riders bond over common interests, cycling becomes both a social outlet and a physical exercise routine",
  
  "Long-distance cycling offers opportunities for personal challenge and endurance development, pushing riders to test their limits and build resilience. These extended rides can strengthen discipline, enhance stamina, and provide a deep sense of achievement. Many cyclists find that such challenges translate to greater confidence and perseverance in other life areas",
  
  "Cycling tourism allows riders to explore new regions at a slower, more immersive pace compared to car travel. Pedaling through scenic landscapes encourages mindful observation and deeper appreciation of local culture, terrain, and nature. This form of travel blends adventure, sustainability, and physical activity into one enriching experience",
  
  "Using a bike instead of a car contributes to environmental sustainability by significantly reducing carbon emissions and energy consumption. Each ride helps decrease air pollution and noise levels in urban areas. Over time, widespread cycling adoption can contribute to cleaner, healthier cities and promote more eco-friendly transportation habits",
  
  "Family cycling outings provide a fun, active way for households to spend time together while encouraging healthy habits for children. These shared rides build positive associations with exercise, promote outdoor exploration, and strengthen family relationships. The approachable nature of cycling makes it easy for people of different ages to participate",
  
  "Participation in local cycling events creates motivation for individuals to train consistently and maintain fitness goals. Whether recreational or competitive, these events foster a sense of accomplishment and community. Riders often develop long-term commitment to active lifestyles as they prepare for and celebrate these shared milestones",
  
  "Cycling enhances spatial awareness as riders navigate varying traffic patterns, terrain changes, and route conditions. This increased attentiveness sharpens overall situational awareness, benefiting both riding safety and everyday decision-making. Over time, cyclists often develop stronger map literacy, road intuition, and attentional focus in diverse environments",
  
  "Regular cycling helps individuals develop discipline through routine training schedules and goal setting. Whether tracking distance, improving speed, or mastering new routes, cyclists often build strong self-management skills. These habits translate beyond physical activity, supporting productivity, perseverance, and personal growth in many areas of life",
  
  "Bike maintenance routines teach practical mechanical skills, giving riders hands-on experience with tools and troubleshooting. Learning to repair flats, adjust brakes, and tune gears fosters confidence and independence. These skills not only ensure safer rides but also provide a satisfying sense of self-reliance and problem-solving capability",
  
  "Cycling supports sustainable urban planning by encouraging cities to develop bike-friendly infrastructure such as protected lanes, bike parking, and multi-use paths. These improvements make communities safer and more accessible for everyone. As cycling culture grows, cities often experience reduced congestion, healthier residents, and more vibrant public spaces"
)


library(reticulate)
library(text)

#I solved it by opening Tools -> Global Options -> Python, clearing the Python interpreter path and unchecking the "Automatically load project-local Python environments" box, t
reticulate::use_condaenv("textrpp_condaenv", required = TRUE)

text::textrpp_initialize(
    condaenv = "textrpp_condaenv",
    refresh_settings = TRUE,
    save_profile = TRUE,
) 

library(tictoc)
tic()
xxx <- dat %>% 
#    dplyr::slice_sample(n = 30) %>% 
    textTopics(., variable_name = "statement", save_dir = "tmp_topic_model")
toc()





# https://www.r-text.org/articles/reticulate.html

reticulate::conda_create("textrpp_condaenv", packages = "python=3.9")
rpp_packages <- c(
    "torch>=2.2.0",
    "transformers>=4.38.0",
    "huggingface_hub>=0.20.0",
    "numpy>=1.26.0",
    "pandas>=2.0.3",
    "nltk>=3.8.1",
    "scikit-learn>=1.3.0",
    "datasets>=2.16.1",
    "evaluate>=0.4.0",
    "accelerate>=0.26.0",
    "bertopic>=0.16.3",
    "jsonschema>=4.19.2",
    "sentence-transformers>=2.2.2",
    "flair>=0.13.0",
    "umap-learn>=0.5.6",
    "hdbscan>=0.8.33",
    "scipy>=1.10.1",
    "aiohappyeyeballs>=2.4.4"
)
reticulate::conda_install("textrpp_condaenv", packages = rpp_packages, pip = TRUE, update_packages = TRUE)



reticulate::use_condaenv("/Users/endress/Library/r-miniconda-arm64/envs/textrpp_condaenv/bin/python", required = TRUE)

library(text)

text::textrpp_initialize(
    condaenv = "textrpp_condaenv",
    refresh_settings = TRUE,
    save_profile = TRUE,
) 

