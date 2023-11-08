#!/usr/local/bin/python3
# -*- coding: utf-8 -*-

import sys
import argparse
import ast
from datetime import datetime
from math import radians, sin, cos, acos
import urllib.request
import xml.etree.ElementTree as xmlTree


class Esdeveniment:
    def __init__(
        self, nom, lloc, carrer, num, latitud, longitud,
            classificacions, dataIni, dataFi, hora, districte, municipi):
        self.nom = nom
        self.lloc = lloc
        self.adreca = carrer
        if (num):
            self.adreca += ' ' + num
        self.latitud = None
        if not latitud == " ":
            self.latitud = latitud
        self.longitud = None
        if not longitud == " ":
            self.longitud = longitud
        self.classificacions = classificacions
        self.dataIni = dataIni
        self.dataFi = dataFi
        self.hora = hora
        self.districte = districte
        self.municipi = municipi
        self.estacionsLliures = []
        self.estacionsAmbBicis = []

    def distancia(self, estacio):
        if (self.latitud and self.longitud):
            lat1 = radians(float(self.latitud))
            lon1 = radians(float(self.longitud))
            lat2 = radians(float(estacio.latitud))
            lon2 = radians(float(estacio.longitud))
            r = abs(6371.01*acos(sin(lat1)*sin(lat2) +
                    cos(lat1)*cos(lat2) * cos(lon1-lon2)))
            return round(r*1000, 2)
        else:
            return 999999

    def displayInfo(self, data):
        print('Nom: ' + self.nom)
        print('  Lloc: ' + self.lloc)
        print('  Districte: ' + self.districte)
        print('  Municipi: ' + self.municipi)
        print('  Adreça: ' + self.adreca)
        if (self.latitud and self.longitud):
            print('  Coordenades: (' + str(self.latitud) + ', ' +
                  str(self.longitud) + ')')
        print('  Classificacions: ')
        for c in self.classificacions:
            print('    ' + c)
        if (self.dataIni):
            print('  Data Inici: ' + self.dataIni.strftime('%d/%m/%Y'))
        if (self.dataFi):
            print('  Data Fi: ' + self.dataFi.strftime('%d/%m/%Y'))
        if (self.hora):
            print('  Hora d\'inici: ' + str(self.hora))
        if data is None:
            print('  Estacions amb espais lliures: ')
            if (self.estacionsLliures and
                    len(self.estacionsLliures) > 0):
                for e in self.estacionsLliures:
                    e[0].displayInfo()
                    print('      A ' + str(e[1]) + ' metres')
            else:
                print('    -')
            print('  Estacions amb bicis disponibles: ')
            if (self.estacionsAmbBicis and len(self.estacionsAmbBicis) > 0):
                for e in self.estacionsAmbBicis:
                    e[0].displayInfo()
                    print('      A ' + str(e[1]) + ' metres')
            else:
                print('    -')
        print()

    def compleix(self, consulta):
        if isinstance(consulta, tuple):
            for c in consulta:
                if self.compleix(c):
                    return True
            return False
        elif isinstance(consulta, list):
            for c in consulta:
                if not self.compleix(c):
                    return False
            return True
        else:
            return consulta in (self.nom + self.lloc + self.adreca +
                                self.districte +
                                str(self.classificacions)).lower()

    def afegirEstacions(self, estacions, distancia):
        self.estacionsLliures = []
        self.estacionsAmbBicis = []
        for e in estacions:
            d = self.distancia(e)
            if d <= distancia:
                if e.slots > 0:
                    self.estacionsLliures.append((e, d))
                if e.bikes > 0:
                    self.estacionsAmbBicis.append((e, d))
        self.estacionsLliures.sort(key=lambda x: x[1])
        self.estacionsAmbBicis.sort(key=lambda x: x[1])
        self.estacionsLliures = self.estacionsLliures[:5]
        self.estacionsAmbBicis = self.estacionsAmbBicis[:5]


class Estacio:
    def __init__(
            self, carrer, num, latitud, longitud, status, slots, bikes):
        self.adreca = carrer
        if (num):
            self.adreca += ' ' + num
        self.latitud = latitud
        self.longitud = longitud
        self.status = status
        self.slots = slots
        self.bikes = bikes

    def displayInfo(self):
        print('    Adreça: ' + self.adreca)
        # print ('Coordenades: (' + str(self.latitud)
        #     + ', ' + str(self.longitud) + ')')
        print('      Status: ' + self.status)
        print('      Slots: ' + str(self.slots))
        print('      Bikes: ' + str(self.bikes))


def obtenirEstacions(urlEstacions):
    opener = urllib.request.build_opener().open(urlEstacions)
    arbreEstacions = xmlTree.parse(opener)
    arrel = arbreEstacions.getroot()
    estacions = []
    for estacio in arrel.findall('station'):
        newEstacio = Estacio(
            str(estacio.find('street').text),
            str(estacio.find('streetNumber').text),
            float(estacio.find('lat').text),
            float(estacio.find('long').text),
            str(estacio.find('status').text),
            int(estacio.find('slots').text),
            int(estacio.find('bikes').text))
        estacions.append(newEstacio)
    return estacions


def obtenirEsdeveniments(urlEsdeveniments, data, consulta):
    opener = urllib.request.build_opener().open(urlEsdeveniments)
    arbreEsdeveniments = xmlTree.parse(opener)
    arrel = arbreEsdeveniments.getroot()
    arrel = arrel.find('body').find('resultat').find('actes')

    esdeveniments = []
    for e in arrel.findall('acte'):
        newEsdeveniment = None
        nom = str(e.find('nom').text)
        lloc = str(e.find('lloc_simple').find('nom').text)
        aux = e.find('lloc_simple').find('adreca_simple')
        carrer = str(aux.find('carrer').text)
        num = str(aux.find('numero').text)
        districte = str(aux.find('districte').text)
        municipi = str(aux.find('municipi').text)
        classificacions = list(map(lambda x: x.text,
                               e.find('classificacions')
                               .findall('nivell')))

        dataIni = None
        dataFi = None
        hora = None
        latitud = None
        longitud = None
        if data is None:
            coords = aux.find('coordenades').find('googleMaps')
            latitud = str(coords.get('lat'))
            longitud = str(coords.get('lon'))
            newEsdeveniment = Esdeveniment(
                nom, lloc, carrer, num, latitud, longitud,
                classificacions, dataIni, dataFi, hora, districte,
                municipi)
        else:
            dates = e.find('data')
            dataIni = dates.find('data_inici').text
            dataFi = dates.find('data_fi').text
            hora = dates.find('hora_inici').text
            if dataIni is not None:
                dataIni = datetime.strptime(str(dataIni), '%d/%m/%Y')
            if dataFi is not None:
                dataFi = datetime.strptime(str(dataFi), '%d/%m/%Y')

            if (
                dataIni is not None and
                dataIni < data and
                ((dataFi is None) or data < dataFi)
            ):
                newEsdeveniment = Esdeveniment(
                    nom, lloc, carrer, num, latitud, longitud,
                    classificacions, dataIni, dataFi, hora, districte,
                    municipi
                )

        if (
            newEsdeveniment is not None and
            newEsdeveniment.compleix(consulta)
        ):
            esdeveniments.append(newEsdeveniment)
    return esdeveniments


def display(list, data):
    if (len(list) > 0):
        if isinstance(list[0], Esdeveniment):
            print('Esdeveniments: ')
            for x in list:
                x.displayInfo(data)
        else:
            print('Estacions: ')
            for x in list:
                x.displayInfo()
    else:
        print('No hi ha esdeveniments que compleixin la consulta!')


def main():
    # Definim arguments
    parser = argparse.ArgumentParser(
        description='Programa per a una seleccio d\'esdeveniments')
    parser.add_argument(
        '--key',
        type=str,
        required=True,
        help='Restriccions de la consulta')
    parser.add_argument(
        '--date',
        type=str,
        help='Data per a buscar activitats (format DD/MM/YYYY)')
    parser.add_argument(
        '--distance',
        type=int,
        default=500,
        help='Distancia per a la busqueda d\'estacions de bicing')
    args = parser.parse_args()

    # Obtenim Consulta
    try:
        consulta = ast.literal_eval(str(args.key))
        print('La consulta obtinguda és: ')
        print('  ' + str(consulta))
    except:
        parser.print_help()
        return

    # Obtenim data o distancia
    data = None
    distancia = 0
    if args.date:
        try:
            data = datetime.strptime(str(args.date), '%d/%m/%Y')
            print('La data entrada és: ')
            print('  ' + datetime.strftime(data, '%d/%m/%Y'))
        except:
            print('Error en l\'entrada de la data')
            parser.print_help()
            return
    else:
        print('Utilitzarem la data d\'avui')
        if (args.distance):
            distancia = args.distance
            print('La distancia per a fer la búsqueda és:')
            print('    ' + str(distancia) + ' metres')

    # Obtenim esdeveniments
    print('OBTENINT ESDEVENIMENTS/ACTES I PROCESSANT ...')
    if data is None:
        urlEsdeveniments = 'http://w10.bcn.es/APPS/'\
            'asiasiacache/peticioXmlAsia?id=199'
    else:
        urlEsdeveniments = 'http://w10.bcn.es/APPS/'\
            'asiasiacache/peticioXmlAsia?id=103'
    esdeveniments = []
    esdeveniments = obtenirEsdeveniments(
        urlEsdeveniments, data, consulta)

    # Obtenim estacions
    if data is None:
        print('OBTENINT ESTACIONS DE BICING I PROCESSANT ...')
        urlEstacions = 'https://wservice.viabicing.cat/'\
            'v1/getstations.php?v=1'
        estacions = obtenirEstacions(urlEstacions)
        # Afegim les estacions als esdeveniments
        for e in esdeveniments:
            e.afegirEstacions(estacions, distancia)

    # Preguntem i generem txt, si cal
    print('Introdueix un nom per a la consulta')
    fileName = input()
    print('Vols generar un fitxer .txt amb TOT el resultat de la'
          ' consulta? [si/no]')
    r = input()
    if r == 'si':
        sys.stdout = open(fileName+'.txt', 'w')
        display(esdeveniments, data)
        sys.stdout = sys.__stdout__

    # Generem html
    html = open(fileName+'.html', 'w')
    html.write('<html><head><meta charset="utf-8"/>'
               '<style>td, th { width: 25%; padding: 10px }</style>'
               '</head>'
               '<table border=1 style="border: 1px solid black;'
               'border-collapse:collapse;">'
               '<tr>'
               '<th>Esdeveniment / Acte</th>'
               '<th>Localització</th>')
    if data is None:
        html.write('<th>Estacions amb bicis disponibles</th>'
                   '<th>Estacions amb espais lliures</th>')
    else:
        html.write('<th>Dia i Hora</th>')
    html.write('</tr>')
    for e in esdeveniments:
        html.write('<tr>')
        html.write('<td><b>' + e.nom + '</b></td>')
        html.write('<td>')
        html.write('<b>' + e.lloc + '</b><ul>')
        html.write('<li>' + e.municipi + '</li>')
        html.write('<li>' + e.districte + '</li>')
        html.write('<li>' + e.adreca + '</li>')
        html.write('</ul></td>')
        if data is None:
            html.write('<td><ul>')
            if (len(e.estacionsAmbBicis) == 0):
                html.write(
                    '<li><p><b>No hi ha estacions amb bicis '
                    'lliures a una distància menor que ' +
                    str(distancia) + ' metres</b></p></li>')
            for x in e.estacionsAmbBicis:
                html.write('<li>')
                html.write('<p><b>' + x[0].adreca + '</b></p>')
                html.write('<ul>')
                html.write('<li>Situat a ' + str(x[1]) + ' metres</li>')
                html.write('<li>Amb ' + str(x[0].bikes) +
                           ' bicis lliures</li>')
                html.write('</ul>')
            html.write('</ul></td>')
            html.write('<td><ul>')
            if (len(e.estacionsLliures) == 0):
                html.write('<li><p><b>No hi ha estacions amb espais '
                           'lliures a una distància menor que ' +
                           str(distancia) + ' metres</b></p></li>')
            for x in e.estacionsLliures:
                html.write('<li>')
                html.write('<h4>' + x[0].adreca + '</h4>')
                html.write('<ul>')
                html.write('<li>Situat a ' + str(x[1]) + ' metres</li>')
                html.write('<li>Amb ' + str(x[0].slots) +
                           ' espais lliures</li>')
                html.write('</ul>')
            html.write('</ul></td>')
        else:
            aux = '<td><ul><li>'
            if (e.dataIni):
                aux += datetime.strftime(e.dataIni, '%d/%m/%Y')
            else:
                aux += 'Indefinit'
            aux += ' - '
            if (e.dataFi):
                aux += datetime.strftime(e.dataFi, '%d/%m/%Y')
            else:
                aux += 'Indefinit'

            if (e.hora):
                aux += '</li><li>' + str(e.hora) + ' h'
            aux = aux + '</li></ul></td>'
            html.write(aux)
        html.write('</tr>')
    html.write('</table></html>')
    html.close()

    # Informem a l'usuari abans d'acabar l'execucióNFORMEM A L'USUARI
    if r == 'si':
        print('Generats els fitxers ' + fileName +
              '.txt i ' + fileName +
              '.html amb el resultat de la consulta')
    else:
        print('Generat el fitxer ' + fileName + '.html amb el resultat'
              ' de la consulta')

main()
