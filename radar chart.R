library(tidyverse)
library(dplyr)
library(ggradar)
library(data.table)

#http://sisisemail.up.edu.pe/sisisemail/docs/2021/169/Los-Objetivos-de-Desarrollo-Sostenible-de-la-Agenda-2030-y-los-Planes-de-Gobierno.pdf

df = read.csv('Agenda 2030.csv')
head(df)
summary(df)


# si hago un analisis por dimensiones primero agrupare y despues transpongo el df para hacer el chart

#Los grupos que usare para comparar

Primeros = c("Accion.Popular", "Avanza.Pais","Juntos.por.el.Peru","Fuerza.Popular",'Peru.Libre','Renovacion.Popular')

mejores_ods =  c("Alianza.para.el.Progreso",'Frente.Amplio',
                 "Juntos.por.el.Peru",'Partido.Morado','Podemos.Peru','Somos.Peru','Victoria.Nacional')

izq  =  c("Accion.Popular","Democracia.Directa","Frente.Amplio",
              "Juntos.por.el.Peru","Partido.Nacionalista", 'Peru.Libre',
              'Runa', 'Union.por.el.Peru')

der  =  c("Avanza.Pais","Fuerza.Popular",'Partido.Popular.Cristiano',
          'Renovacion.Popular','Somos.Peru')

centro = c("Alianza.para.el.Progreso",'Partido.Morado',
           'Peru.Patria.Segura', 'Podemos.Peru','Victoria.Nacional')

todxs = c("Alianza.para.el.Progreso","Accion.Popular",
                 "Avanza.Pais","Democracia.Directa","Frente.Amplio",
                 "Juntos.por.el.Peru","Fuerza.Popular",'Partido.Morado',
                 "Partido.Nacionalista", 'Peru.Libre',
                 'Peru.Patria.Segura', 'Podemos.Peru','Partido.Popular.Cristiano',
                 'Renovacion.Popular','Somos.Peru','Victoria.Nacional',
                 'Runa', 'Union.por.el.Peru')

#DIMENSIONES
#################


df_d = df%>%
  group_by(Dimensiones)%>%
  summarise_at(Primeros, sum)

df_d = transpose(df_d,keep.names='Dimensiones',make.names = 'Dimensiones'	)


summary(df_d)

### radar chart

ggradar(
  df_d, 
  values.radar = c(0,"10","15"),
  grid.min = 0, grid.mid = 10, grid.max = 15,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#cb3234","#252850","#ff8000","#00bb2d","#ffff00", '#51d1f6'),
  background.circle.colour = "white",
  gridline.min.colour = "black",
  gridline.mid.colour = "black",
  gridline.max.colour = "black",
  plot.title = 'Planes de Gob y Agenda 2030 x dimension - 6 Primeros',
  legend.title = '',
  legend.text.size = 10,
  legend.position = 'right',
  axis.label.size = 4,
  base.size = 5,
  grid.label.size = 5
)



#EJES
################# 


df_e = df%>%
  group_by(Ejes)%>%
  summarise_at(Primeros, sum)

df_e = transpose(df_e,keep.names='Ejes',make.names = 'Ejes'	)


summary(df_e)

### radar chart

ggradar(
  df_e, 
  values.radar = c(0,"10","20"),
  grid.min = 0, grid.mid = 10, grid.max = 20,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#cb3234","#252850","#ff8000","#00bb2d","#ffff00", '#51d1f6'),
  background.circle.colour = "white",
  gridline.min.colour = "black",
  gridline.mid.colour = "black",
  gridline.max.colour = "black",
  plot.title = 'Planes de Gob y Agenda 2030 por Ejes - 6 Primeros',
  legend.title = '',
  legend.text.size = 10,
  legend.position = 'right',
  axis.label.size = 4,
  base.size = 5,
  grid.label.size = 5
)

#Todos las objetivos
################## 


tres = c(
          "Juntos.por.el.Peru",
         'Peru.Libre')


df_t = df%>%
  group_by(ODS)%>%
  summarise_at(tres, sum)

df_t = transpose(df_t,keep.names='ODS',make.names = 'ODS'	)


summary(df_t)

### radar chart

ggradar(
  df_t, 
  values.radar = c(0,"2","4.5"),
  grid.min = 0, grid.mid = 2, grid.max = 4.5,
  group.line.width = 1, 
  group.point.size = 3,
  group.colours = c("#00bb2d", "#ff0000"),
  background.circle.colour = "white",
  gridline.min.colour = "black",
  gridline.mid.colour = "black",
  gridline.max.colour = "black",
  plot.title = 'JP vs PL segun los Objetivos Agenda 2030',
  legend.title = '',
  legend.text.size = 10,
  legend.position = 'bottom',
  axis.label.size = 3.5,
  base.size = 2,
  grid.label.size = 5
)

#todo por Por dimensiones
###############
ggplot(df,)



