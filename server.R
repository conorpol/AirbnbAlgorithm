server <- function(input, output, session) {
    
    observeEvent(input$data,{
        output$data <- renderText({
            paste("zip:  ", input$zip)
        })
    })

    
    # Output Zipcode
    output$selected_var <- renderText({
        paste("Zipcode: ", input$var)
    })
    
    # Output Cluster Type
    output$selected_type <- renderText({
        paste("Property Type: ", input$type)
    })
    
    #Output Square Meter
    output$selected_integer = renderText({
        paste("Square Meter: ", input$integer)
    })
    
    #Output Price
    output$selected_price = renderText({
        paste("Price: ", input$price)
    })
    
    # Output Accomodates
    output$amens = renderText({
        paste("Amenities: ", input$fam)
    })
    

    ####### Predictions #######
    
    observeEvent(input$Enter,{
        zip = input$zip
        smeter = input$sq
        accom = input$acc
        clust = input$clust
        type = input$type
        bedr = input$bedroom
        bath = input$bath
        beds = input$bed
        famfriend = input$fam
        tv = input$tv
        sham = input$sham
        ac = input$ac
        desk = input$desk
        iron = input$iron
        haird = input$hair
        kitch = input$kitch
        
        famfriendNum = ifelse(famfriend,1,0)
        tvNum = ifelse(tv,1,0)
        shamNum = ifelse(sham,1,0)
        acNum = ifelse(ac,1,0)
        deskNum = ifelse(desk,1,0)
        ironNum = ifelse(iron,1,0)
        hairNum = ifelse(haird,1,0)
        kitchNum = ifelse(kitch,1,0)
        
        clusterNum = ifelse(clust == "Full-time (as often as possible)",3,
                            ifelse(clust == "Regularily (1-2 weeks per month)",2,
                                   ifelse(clust == "Occasionally (less than a week per month)",1,"ERROR")
                            )
                     )
        
        t = data.frame(zipcode = zip, accommodates = accom, bathrooms = bath, bedrooms = bedr, 
                       beds = beds, hasTV = tvNum, hasShampoo = shamNum, hasAC = acNum, hasDesk = deskNum, 
                       hasIron = ironNum, hasHairDryer = hairNum, hasKitchen = kitchNum, isFamilyFriendly = famfriendNum,
                       square_meter = smeter, cluster = clusterNum, propertyType = type)
        
        # numeric vars
        t$accommodates = as.numeric(t$accommodates)
        t$bathrooms = as.numeric(t$bathrooms)
        t$bedrooms = as.numeric(t$bedrooms)
        t$beds = as.numeric(t$beds)
        t$square_meter = as.numeric(t$square_meter)
        
        # factor vars
        t$zipcode = as.factor(t$zipcode)
        t$hasTV = as.factor(t$hasTV)
        t$hasShampoo = as.factor(t$hasShampoo)
        t$hasAC = as.factor(t$hasAC)
        t$hasDesk = as.factor(t$hasDesk)
        t$hasIron = as.factor(t$hasIron)
        t$hasHairDryer = as.factor(t$hasHairDryer)
        t$hasKitchen = as.factor(t$hasKitchen)
        t$isFamilyFriendly = as.factor(t$isFamilyFriendly)
        t$cluster = as.factor(t$cluster)
        t$propertyType = as.factor(t$propertyType)
        
        # fixing factor levels
        t$zipcode <- factor(t$zipcode, levels = levels(importantVars$zipcode))
        t$hasTV <- factor(t$hasTV, levels = levels(importantVars$hasTV))
        t$hasShampoo <- factor(t$hasShampoo, levels = levels(importantVars$hasShampoo))
        t$hasAC <- factor(t$hasAC, levels = levels(importantVars$hasAC))
        t$hasDesk <- factor(t$hasDesk, levels = levels(importantVars$hasDesk))
        t$hasIron <- factor(t$hasIron, levels = levels(importantVars$hasIron))
        t$hasHairDryer <- factor(t$hasHairDryer, levels = levels(importantVars$hasHairDryer))
        t$hasKitchen <- factor(t$hasKitchen, levels = levels(importantVars$hasKitchen))
        t$isFamilyFriendly <- factor(t$isFamilyFriendly, levels = levels(importantVars$isFamilyFriendly))
        t$cluster <- factor(t$cluster, levels = levels(importantVars$cluster))
        t$propertyType <- factor(t$propertyType, levels = levels(importantVars$propertyType))
        
        #appForest = randomForest(IncomePerMonth~., ntree=500, data=importantVars, importance=TRUE, do.trace = 100)
        
        output$dynamicText = renderText({
            paste("<font size=4 color=black> Expected Monthly Income is: </font> <font size=4 color=orange><b>", round(predict(fullForest,newdata = t),1)," € </b></font> ")
        })
        
        output$plot = renderPlot({
            plot(importantVars$zipcode, importantVars$IncomePerMonth, col = "dodgerblue2", ylim=c(0,4000), cex = 0.8,main = "Income Per Month by District", xlab="zipcode", ylab="Income Per Month")
            points(t$zipcode, predict(fullForest,newdata=t), col = "orangered",pch = 18, cex = 2)
            legend("topright",c("Your Predicted Income"),pch=18,col ="orangered",cex=1.2, bg = "slategray1")
        })
        
    })
    
    
    ############## Heatmap Code #################
    data <- reactive({
        x <- air2
    })
    
    output$mymap <- renderLeaflet({
        air2 = data()
        
        pal <- colorFactor(c("red", "green", "blue", "yellow", "orange", "mediumvioletred", 
                             "mediumpurple4", "navy","black", "palevioletred1", "lawngreen","lightpink3","sienna4","peru",
                             "brown","burlywood","darkgreen","goldenrod4","darkviolet","lightslateblue"), 
                           domain = c("75001", "75002","75003","75004","75005","75006","75007","75008",
                                      "75009","75010","75011","75012","75013","75014","75015","75016",
                                      "75017","75018","75019","75020"))
        labs <- as.list(air2$zipcode)
        m <- leaflet(data = air2) %>%
            addTiles() %>%
            addCircleMarkers(lng = ~Longitude,
                             lat = ~Latitude,
                             radius = 6,
                             label = lapply(labs, HTML),
                             color = ~pal(zipcode),
                             stroke = FALSE, fillOpacity = 0.5)
        
    })
    
    
}

# APP DEPOLYMENT #

#install.packages("rsconnect")
#library(rsconnect)
#rsconnect::setAccountInfo(appName="airbnbpredictor", token='280E59F64B6253B66213DF92E9BF8D35', secret='VsneoHWLJpI9Cced43ZHILUotYl34Cw47FzWU8iL')
#rsconnect::deployApp('~/Desktop/MGSC401/app')
#deployApp(appName = "airbnbpredictor")
#rsconnect::showLogs()
