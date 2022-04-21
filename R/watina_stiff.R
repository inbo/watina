#' @title get chemical data from Watina database  and return Stiff diagramms
#' @description  The function can only be used from within the INBO network (or via VPN).
#' @export watina_stiff
#' @import watina
#' @import smwrBase
#' @import smwrGraphs
#' @import tidyverse
#' @param watina connection to watina database, default = connect_watina()
#' @param area_code area of interest 3-lettercode (ex. "ZWA"). All piezometers within this area and with a max. filter depth of 5m are taken into account.
#' @param startdate startdate of samples, format = "DD/MM/YYYY"
#' @param enddate enddate, default is current date, format = "DD/MM/YYY"
#' @param format export format of stiff graphs, default is "pdf", other option is "png".
#' @examples
#' \dontrun{watina_stiff(area_code = "ZWA", startdate = "01/01/2017",enddate = "01/01/2020",format = "png")}
#' \dontrun{watina_stiff(area_code = "ZWA", startdate = "01/01/2017",format = "png")}


watina_stiff <- function (watina = connect_watina(),
                          area_code = NULL,
                          startdate = NULL,
                          enddate = paste(day(today()), month(today()), year(today())),
                          format = "pdf") {
  {
    variables_stiff = c("HCO3","SO4", "Cl", "Na", "K", "Ca", "Mg", "Fe")
    watina <- connect_watina()
    locs=get_locs(watina, area_codes = area_code, loc_type = "P",filterdepth_range = c(0,5))
    stiff_input=get_chem(watina,locs=locs,startdate = startdate,en_range = c(-0.1,0.1),enddate = enddate )%>%filter(chem_variable %in% variables_stiff)%>%
      filter(between(elneutr, -0.10, 0.10))%>%
      group_by(loc_code,date,lab_sample_id,chem_variable) %>% summarise(value = mean(value))%>%
      ungroup()%>% collect
    stiff_input = stiff_input %>% pivot_wider(names_from = chem_variable, values_from = value, values_fill = NA)
    mg_to_meq <- transform(stiff_input,
                           Ca.meq = conc2meq(Ca, "calcium"),
                           Mg.meq = conc2meq(Mg, "magnesium"),
                           Na.meq = conc2meq(Na, "sodium"),
                           Cl.meq = conc2meq(Cl, "chloride"),
                           SO4.meq = conc2meq(SO4, "sulfate"),
                           HCO3.meq = conc2meq(HCO3, "bicarb"),
                           K.meq = conc2meq(K, "potassium"))%>%
      mutate("Na+K.meq" = Na.meq + K.meq)
    mg_to_meq$SS <- row.names(mg_to_meq)
    mg_to_meq}

  #still need to fix the margins along the y-axis
  if (format == "png"){
    for (i in mg_to_meq$SS){loop = mg_to_meq %>% filter(SS == i)
    plots=setPNG(paste0(loop$loc_code,"_",loop$date,".png"), width =7, height= 7)
    setGraph(1, AA.lo)
    setTopMargin(margin = c(0,0,0,0))
    AA.pl <- with(loop, stiffPlot(cbind(`Na+K.meq`, Mg.meq, Ca.meq),cbind(Cl.meq, SO4.meq, HCO3.meq), ylabels=paste0(loc_code, "\n",date),hjust=10))
    graphics.off()
    }
    expl =setPNG(paste0("explanation",".png"), width =7, height= 7)
    AA.lo <- setLayout(height=2.5, explanation=list(bottom=2))
    setGraph(1, AA.lo)
    AB.pl= mg_to_meq %>% filter(mg_to_meq$SS==1) %>% with( .,stiffPlot(cbind(`Na+K.meq`, Mg.meq, Ca.meq),cbind(Cl.meq, SO4.meq, HCO3.meq), ylabels=paste0(loc_code, "\n",date)))
    setGraph("explanation", AA.lo)
    addExplanation(AB.pl)
    graphics.off()
    return (list(plots, expl))
  }

  if (format == "pdf"){

    plots = setPDF(basename = paste0(area_code,"_stiffdiagrams"),layout="landscape")
    par(mfrow = c(4,4))
    for (i in mg_to_meq$SS){loop = mg_to_meq %>% filter(SS == i)
    AA.pl <- with(loop, stiffPlot(cbind(`Na+K.meq`, Mg.meq, Ca.meq),cbind(Cl.meq, SO4.meq, HCO3.meq), ylabels=paste0(loc_code, "\n                                       ",date)))}
    expl =setPDF(basename ="explanation",layout="landscape")
    AA.lo <- setLayout(height=2.5, explanation=list(bottom=2))
    setGraph(1, AA.lo)
    AB.pl= mg_to_meq %>% filter(mg_to_meq$SS==1) %>% with( .,stiffPlot(cbind(`Na+K.meq`, Mg.meq, Ca.meq),cbind(Cl.meq, SO4.meq, HCO3.meq), ylabels=paste0(loc_code, "\n",date)))
    setGraph("explanation", AA.lo)
    addExplanation(AB.pl)
    graphics.off()
    return (list(plots, expl))
  }
}
