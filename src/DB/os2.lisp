;;; -*- Mode: Lisp; Syntax: Ansi-Common-Lisp; Package: DATABASE; Base: 10 -*-

(in-package database)

(defparameter *os*
    '(("*** Strassen-Splitter ***" 8888 linie 1 1)
      
      ("Zollgrenze" 75 17 text 1 20)
      ("Abrenzung der BAB, usw. geg. Ausfahrt, unsichtbar" 3105 linie 1 3)
      ("Abgrenzung flaechenhafter Gewaesser, unsichtbar" 5005 linie 1 13)
      ("Abgrenzung zu anderen Ebenen, unsichtbar" 3805 linie 1 5)
      ("Acker" 4114 flaeche 1 18)
      ("AKN-Bahn" 3548 symbol 4 6)
      ("AKN-Bahn, oberirdisch" 3241 linie 1 10)  
      ("AKN-Bahn, oberirdisch" (3241 2002) linie 1 11)
      ("AKN-Station" 3248 symbol 1 10)
      ("Auffahrt auf BAB, BAB-aehnlich, B" 3152 linie 1 3)
      ("Auffahrt auf BAB, BAB-aehnlich, B unter Eb 6 und 7" 3852 1 5)
      ("Autobahn-Anschluss" 3517 text 4 2)
      ("Autobahnkreuz" 3507 text 4 2)
      
      ("B-Symbol (Bundesstrassenschild)" 3138 symbol-mit-text 1 2)
      ("BAB" 3114 flaeche 1 3)
      ("BAB unter Eb. 6 und 7" 3814 flaeche 1 5)
      ("BAB-aehnlich" 3124 flaeche 1 3)
      ("BAB-aehnlich unter Eb. 6 und 7" 3824 flaeche 1 5)
      ("BAB-aehnlich, Mittellinie" 3123 linie 1 3)
      ("BAB-aehnlich, Mittellinie" (3123 2002) linie 1 2)
      ("BAB-aehnlich, Mittellinie unter Eb 6 und 7" 3823 linie 1 4)
      ("BAB-aehnlich, Randbegrenzung" 3122 linie 1 3)
      ("BAB-aehnlich, Randbegrenzung unter Eb 6 und 7" 3822 linie 1 5)
      ("BAB-Anschlusstelle" 3157 text 1 2)
      ("BAB-Kreuz" 3117 text 1 2)
      ("BAB-Mittellinie" 3113 linie 1 2)
      ("BAB-Mittellinie" (3113 2002) linie 1 3)
      ("BAB-Mittellinie unter Eb 6 und 7" 3813 linie 1 4)
      ("BAB-Randbegrenzung" 3112 linie 1 3)
      ("BAB-Randbegrenzung unter Eb 6 und 7" linie 1 5)
      ("BAB-Schild" 3568 symbol-mit-text 4 6)
      ("BAB-Symbol" 3118 symbol-mit-text 1 2)
      ("BAB-Tunnel, Randbegrenzung" 3612 linie 1 2)
      ("Bahnbetriebsgelaende" 3264 flaeche 1 11)
      ("Baublockgrenze" 7291 linie 2 10)
      ("Baublockgrenze, im Gewaesser" 7295 linie 2 10)
      ("Baublocknummer" 7297 text 2 20)
      ("Baublockzusammengehoerigkeit" 7298 symbol 2 12)
      ("Berg-Hoehenzahl" 7309 text 1 18)
      ("Bergname" 7307 text 1 18)
      ("Bezirksgrenzband, Abgrenzung aussen (60 m)" 7222 linie 2 21)
      ("Bezirksgrenzband, Abgrenzung Mitte (30 m)" 7223 linie 2 21)
      ("Bezirksgrenze" 7221 linie 2 3)
      ("Bezirksgrenze, Grenzband 1" 7224 flaeche 2 21)
      ("Bezirksgrenze, Grenzband 2" 7226 flaeche 2 21)
      ("Bezirksname" 7227 text 1 24)
      ("Blattschnitt (Bearbeitungsgrenze fuer Schrift)" 1131 linie 1 24)
      ("Brachflaeche" 4404 flaeche 1 18)
      ("Bruecke" 3009 linie 1 6)
      ("Brueckenname" 3007 text 1 6)
      ("Bundesbahn, unterirdisch" (3711 2002) linie 1 11)
      ("Bundesbahn, oberirdisch" 3211 linie 1 10)
      ("Bundesbahn, oberirdisch" (3211 2002) linie 1 11)
      ("Bundesbahn, unterirdisch" 3711 linie 1 10)
      ("Bundesstrasse" 3134 flaeche 1 3)
      ("Bundesstrasse" 3132 linie 1 3)
      ("Bundesstrasse" 3558 symbol-mit-text 4 6)
      ("Bundesstrasse unter Eb 6 und 7" 3834 flaeche 1 5)
      ("Bundesstrasse unter Eb 6 und 7" 3832 linie 1 5)
      ("Busendhaltestelle" 3206 symbol-mit-text 3 10)
      ("Buslinie" 3201 linie 3 10)
      ("Buslinien-Nummer" 3204 text 3 10)
      ("Busstation" 3200 symbol 3 10)
      
      ("Campingplatz" 2244 flaeche 1 18)
      ("Campingplatz" 2248 symbol 1 18)
      
      ("Deich, Damm" 4011 linie 1 18)
      ("Deich, Damm" 4018 symbol-an-linie 1 18)
      ("Denkmal" 9308 symbol 1 19)
      ("DGK-5-Bezeichnung" 1118 text 1 1)
      ("Dock" 5724 flaeche 1 13)
      ("Dock" 5721 linie 1 13)
      
      ("Europastrasse" 3578 symbol-mit-text 4 6)
      ("Europastrasse (Schild)" 3109 symbol-mit-text 1 2)
      
      ("Fernsehturm" 9418 symbol 1 19)
      ("Feuerwehr" 8271 symbol 1 19)
      ("Feuerwehr" 8558 symbol 4 6)
      ("flaechenhafte Gewaesserbegr., schiffbar, unt. Ebene 6 und 7" 5302 linie 1 15)
      ("flaechenhafte Gewaesserbegr., nicht schiffbar, unt. Ebene 6 und 7" 5402 linie 1 15)
      ("flaechenh. Gewaesserbegr., nicht schiffbar" 5202 linie 1 13)
      ("flaechenh. Gewaesserbegr., schiffbar" 5102 linie 1 13)
      ("Fliessrichtungspfeil" 5018 symbol 1 12)
      ("Flughafen" 3318 symbol 1 25)
      ("Flughafen- topographie" 3309 linie 1 25)
      ("Fluss, nicht schiffbar" 5234 flaeche 1 13)
      ("Fluss, nicht schiffbar, unter Eb 6 und 7" 5434 flaeche 1 15)
      ("Fluss, schiffbar" 5134 flaeche 1 13)
      ("Fluss, schiffbar, unter Eb 6 und 7" 5334 flaeche 1 15)
      ("Freihafengrenze" 7511 linie 1 20)
      ("Friedhof" 2254 flaeche 1 18)
      ("Friedhof" 2258 symbol 1 18)
      ("Friedhof fuer Nichtchristen" 2256 symbol 1 18)
      ("Friedhofsname" 2257 text 1 18)
      ("Funkmast" 9408 symbol 1 19)
      ("Fussgaengerzone" 3194 flaeche 1 7)
      
      ("Gehoelz" 4254 flaeche 1 18)
      ("Gemeinde/Stadtteil" 7527 text 4 3)
      ("Gemeindegrenze" 7271 linie 2 8)
      ("Gemeindename (auch Name einer Samtgemeinde)" 7277 text 2 18)
      ("Gewaechshaus" 4154 flaeche 1 18)
      ("Gewaesser" 5517 text 4 5)
      ("Gewaessername" 5017 text 1 12)
      ("Gitterpunkte fuer das Einpassen von Vorlagen" 1119 punkt 2 1)
      ("GK-Gitterlinie 1 km" 1121 linie 2 1)
      ("GK-Gitterlinie 2 km" 1111 linie 2 1)
      ("Grenzen einer Mitgliedsgemeinde einer Samtgemeinde" 7276 linie 2 8)
      ("Grenze statistisches Gebiet" 7281 linie 2 9)
      ("Grenze statistisches Gebiet, gleichz. Ortsteilgr." (7281 7215) linie 2 11)
      ("Grenze statistisches Gebiet, im Gewaesser" 7285 linie 2 9)
      
      ("Hafen" 5254 flaeche 1 13)
      ("Hafen unter Eb 6 und 7" 5354 flaeche 1 15)
      ("Hafengebietsgrenze (Zustaendigkeit Strom- und Hafenbau)" 7652 linie 2 27)
      ("Hauptverkehrsstrasse, Tunnel" 3642 linie 1 2)
      ("Hauptverkehrsstrasse" 3142 linie 1 3)
      ("Hauptverkehrsstrasse unter Eb 6 und 7" 3842 linie 1 5)
      ("Hausnummer" 2327 text 1 19)
      ("Heide" 4344 flaeche 1 18)
      ("Heide" 4348 symbol 1 18)
      ("Hilfslinie" 0000 linie 1 6)
      ("Hochspannungsleitung" 9001 linie 1 22)
      ("Hochspannungsleitung" 9007 text 1 22)
      ("Hochspannungsmast" 9008 symbol 1 22)
      ("Hochspannungspfeil" 9018 symbol 1 22)
      ("Hochwasserschutzanlage (nicht Deich 4011/18!)" 5821 linie 1 12)
      
      ("Industrie/Gewerbe" 2140 flaeche 1 17)
      
      ("Kanal, nicht schiffbar" 5244 flaeche 1 13)
      ("Kanal, nicht schiffbar, unter Eb 6 und 7" 5444 flaeche 1 15)
      ("Kanal, schiffbar" 5144 flaeche 1 13)
      ("Kanal, schiffbar, unter Eb 6 und 7" 5344 flaeche 1 15)
      ("Kapelle" 8141 symbol 1 19)
      ("Kilometrierung BAB, BAB-aehnlich" 3108 symbol 1 2)
      ("Kilometrierung BAB, BAB-aehnlich" 3107 text 1 2)
      ("Kirche" 8140 symbol 1 19)
      ("Kirche" 8540 symbol 4 6)
      ("Kleingarten" 2234 flaeche 1 18)
      ("Kleingarten" 2238 symbol 1 18)
      ("Kleingartennummer(-name)" 2237 text 1 18)
      ("Kontur fuer oeffentliches Gebaeude" 2312 linie 1 19)
      ("Krankenhaus" 8151 symbol 1 19)
      ("Kreisgrenze" 7261 linie 2 7)
      ("Kreisname" 7267 test 2 17)
      ("Kuestenbereich" 4334 flaeche 1 18)
      ("Kuestenbereich" 4338 symbol-an-linie 1 18)
      
      ("Landesgrenzband, Abgrenzung aussen (100 m)" 7212 linie 2 21)
      ("Landesgrenzband, Abgrenzung Mitte (50 m)" 7213 linie 2 21)
      ("Landesgrenze" 7211 linie 2 2)
      ("Landesgrenze, Grenzband 1" 7214 flaeche 2 21)
      ("Landesgrenze, Grenzband 2" 7216 flaeche 2 21)
      ("Landschaftsname" 7217 text 1 18)
      ("Landungsbruecke, Anlegestelle" 5831 linie 1 12)
      ("Landungsbruecke, Anlegestelle" (5831 2002) linie 1 13)
      ("Laubbaum" 4218 symbol 1 18)
      ("Laubholz" 4268 symbol 1 18)
      ("Laubwald" 4214 flaeche 1 18)
      ("linienh. Kanal, Graben, schiffbar, unter Eb 6 und 7" (5321 2002) linie 1 15)
      ("linienh. Kanal, Graben, nicht schiffbar, unter Eb 6 und 7" 5421 linie 1 14)
      ("linienh. Kanal, Graben, nicht schiffbar, unter Eb 6 und 7" (5421 2002) linie 1 15)
      ("linienh. Kanal, Graben, schiffbar, unter Eb 6 und 7" 5321 linie 1 14)
      ("linienh. Bach, nicht schiffbar" 5211 linie 1 12)
      ("linienh. Bach, nicht schiffbar" (5211 2002) linie 1 13)
      ("linienh. Bach, nicht schiffbar, unter Eb 6 und 7" 5411 linie 1 14)
      ("linienh. Bach, nicht schiffbar, unter Eb 6 und 7" (5411 2002) linie 1 15)
      ("linienh. Kanal, Graben, nicht schiffbar" (5221 2002) linie 1 13)
      ("linienh. Kanal, Graben, nicht schiffbar" 5221 linie 1 12)
      ("linienh. Kanal, Graben, schiffbar" 5121 linie 1 12)
      ("linienh. Kanal, Graben, schiffbar" (5121 2002) linie 1 13)
      
      ("Mischwald" 4234 flaeche 1 18)
      ("Mole" 5841 linie 1 13)
      ("Moor, Sumpf" 4314 flaeche 1 18)
      ("Moor, Sumpf" 4318 symbol 1 18)
      
      ("Nadelbaum" 4228 symbol 1 18)
      ("Nadelholz" 4258 symbol 1 18)
      ("Nadelwald" 4223 flaeche 1 18)
      ("Name des Naturschutzgebiets" 7427 text 1 23)
      ("Name einer Mitgliedsgemeinde einer Samtgemeinde" 7270 text 2 18)
      ("Name fuer Kai, Hoeft, Anlegestelle" 5837 text 1 12)
      ("Name wichtiger oeffentlicher Gebaeude" 2317 text 1 19)
      ("Naturschutzgebietsgrenze" 7422 linie 1 23)
      ("Nebenbahn (Hafenb., Rangieranl. usw.), unterirdisch" 3751 linie 1 10)
      ("Nebenbahn (Hafenb., Rangieranl. usw.), oberirdisch" 3251 linie 1 10)
      ("Nebenbahn (Hafenb., Rangieranl. usw.), oberirdisch" (3251 2002) linie 1 11)
      ("Nummer eines statistischen Gebiets" 7287 text 2 19)
      ("Nutzungsartengrenze, unsichtbar" 9005 linie 1 16)
      ("Nutzungsartengrenze" 2002 linie 1 16)
      
      ("Obstanbau" 4134 flaeche 1 18)
      ("Ortsamtsgebietsgrenze" 7231 linie 2 4)
      ("Ortsamtsgebietsname" 7237 text 2 14)
      ("Ortschaften/Ortsteil" 7557 text 4 3)
      ("Ortsteilgrenze" 7251 linie 2 6)
      ("Ortsteilname" 7257 text 2 16)
      ("Ortsteilnummer" 7259 text 2 26)
      
      ("oeffentliches Gebaeude" 2314 flaeche 1 19)
      
      ("P+R-Symbol" 3208 symbol 1 10)
      ("Park, mit Einzelsymbolen" 2224 flaeche 1 18)
      ("Parksname" 2227 text 1 18)
      ("Parkplatz" 3100 flaeche 1 9)
      ("Platz, z.B. Marktplatz, Rastplatz, Festplatz" 3104 flaeche 1 9)
      ("Platzname" 3106 text 1 9)
      ("Polizei" 8171 symbol 1 19)
      ("Postamt" 8113 symbol 1 19)
      
      ("Ring 1-3 (Schild)" 3148 symbol-mit-text 1 2)
      
      ("S-Bahn" 3528 symbol 4 6)
      ("S-Bahn, oberirdisch" 3221 linie 1 10)
      ("S-Bahn, oberirdisch" (3221 2002) linie 1 11)
      ("S-Bahn, unterirdisch" 3721 linie 1 10)
      ("S-Bahn, unterirdisch" (3721 2002) linie 1 11)
      ("S-Bahn-Station" 3228 symbol 1 10)
      ("Samtgemeindegrenze" 7279 linie 2 8)
      ("Sandbank" 5714 flaeche 1 13)
      ("Schiffahrtslinie" 3411 linie 1 12)
      ("Schiffsverkehr (Be- und Entladen)" 3424 flaeche 1 13)
      ("Schleuse" 5811 linie 1 12)
      ("Schleusenname" 5817 text 1 12)
      ("See, nicht schiffbar" 5264 flaeche 1 13)
      ("See, nicht schiffbar, unter Eb 6 und 7" 5464 flaeche 1 15)
      ("See, schiffbar" 5164 flaeche 1 13)
      ("See, schiffbar, unter Eb 6 und 7" 5364 flaeche 1 15)
      ("Seegras" 4358 symbol 1 18)
      ("Segelflughafen" 3328 symbol 1 25)
      ("Sonderbezeichnung (z.B. Wandsbek-Gartenstadt)" 7207 text 2 22)
      ("Sonstige Strasse" 3162 linie 1 7)
      ("Sontige Strasse, Tunnel" 3662 linie 1 6)
      ("Sportplatz" 2218 symbol 1 18)
      ("Sportplatzbegrenzung" 2212 linie 1 18)
      ("Sportplatzinnenflaeche" 2214 flaeche 1 18)
      ("Sportplatzname" 2217 text 1 18)
      ("Stadtteilgrenze" 7241 linie 2 5)
      ("Stadtteilname" 7247 text 2 15)
      ("Stationsname" 3207 text 1 10)
      ("Strassenname, BAB-aehnlich" 3127 text 1 2)
      ("Strassenname, Bundesstrasse" 3137 text 1 2)
      ("Strassenname, Hauptverkehrsstrasse" 3147 text 1 2)
      ("Strassenname" 3167 1 6)
      
      ("Teich, nicht schiffbar" 5274 flaeche 1 13)
      ("Teich, nicht schiffbar, unter Eb 6 und 7" 5474 flaeche 1 15)
      ("topographische Grenze" 4002 linie 1 16)
      ("Treppe" 3168 symbol 1 6)
      ("Treppenbegrenzung" 3171 linie 1 6)
      
      ("U-Bahn" 3538 symbol 4 6)
      ("U-Bahn, oberirdisch" 3231 linie 1 10)
      ("U-Bahn, oberirdisch" (3231 2002) 1 11)
      ("U-Bahn, unterirdisch" 3731 linie 1 10)
      ("U-Bahn, unterirdisch" (3731 2002) linie 1 11)
      ("U-Bahn-Station" 3238 symbol 1 10)
      
      ("Verkehrsbegleitgruen" 4004 flaeche 1 18)

      ("Wanderweg" 9803 linie 1 27)
      ("Wanderwegbezeichnung" 9808 symbol 1 27)
      ("Watt" 5214 flaeche 1 6)
      ("Weg, Hintegrund in Flaechenfarbe" 3182 linie 1 6)
      ("Wiese, Weide" 4124 flaeche 1 18)
      ("Wiese, Weide" 4128 symbol 1 18)
      ("Wohnen" 2110 flaeche 1 17)
      
      ("Zoo" 2264 flaeche 1 18)
      ("Zustandsbezeichnung, z.B. Container-Terminal, o.ae." 7337 text 1 18)
      ("zusaetzliche Topographie (keine Nutzungsgrenze)" 9002 linie 1 16)))

(setf *os* 
  (sort *os*
	'string< :key #'(lambda (i) (string-upcase (first i)))))

;;;
;;;
;;;

(defparameter *how-to-interpret-os*
    '( ;;; Top Level Polygone = FL => in Segmente zerlegen, Polygone finden
       ;;; die einzigen "ganzen" Objekte
      
      ;;; Format: 
      ;;; 1: OS 2: Name 3: SQD-Typ(en) 4: transformierte(r) DB-Typ(en) 5: Konstruktor-Funktion(en)
      ;;;
      
      (nil nil PG db-point pg-create-point)
      
      (3147 "Strassenname" TX map-text tx-create-map-text)
      (3207 "Stationsname" TX map-text tx-create-map-text)
      (2317 "Name von wichtigem oeffentlichen Gebaeude" TX map-text tx-create-map-text)
      (5017 "Gewaessername" TX map-text tx-create-map-text)
      (2327 "Hausnummer" TX map-text tx-create-map-text)
      (3167 "Strassenname" TX map-text tx-create-map-text)
      
      (4114 "Acker" FL db-polygon fl-create-containing-polygon) 
      (2110 "Wohngebiet" FL db-polygon fl-create-containing-polygon)
      (4124 "Wiese/Weide" FL db-polygon fl-create-containing-polygon)
      (4004 "Verkehrsbegleitgruen" FL db-polygon fl-create-containing-polygon)
      (5274 "Teich" FL db-polygon fl-create-containing-polygon)
      (2214 "Sportplatz" FL db-polygon fl-create-containing-polygon)
      (3104 "Marktplatz" FL db-polygon fl-create-containing-polygon)
      (2224 "Parkplatz" FL db-polygon fl-create-containing-polygon)
      (2224 "Park" FL db-polygon fl-create-containing-polygon)
      (2314 "Schule" FL db-polygon fl-create-containing-polygon)
      (2234 "Kleingartengebiet" FL db-polygon fl-create-containing-polygon)
      (2140 "Gewerbegebiet" FL db-polygon fl-create-containing-polygon)
      (2254 "Friedhof" FL db-polygon fl-create-containing-polygon)
      (4404 "Brachflaeche" FL db-polygon fl-create-containing-polygon)
      (3114 "BAB" FL db-line fl-do-nothing)
      
      ;;; Top Level Linien = SN oder LI => alle SN in Segmente zerlegen (=> Ketten bilden ?)
      ;;; xxx* = Teilstueck von "xxx" (z.B. Segment = Teilstueck eines Baches, 
      ;;; oder Segment = Teilstueck des Randes eines Polygones) 
      
      (5211 "Bach*" (LI BO SN) db-linie
       (li-create-line bo-create-line sn-create-segment-list))
      
      (3182 "Weg*" (LI BO SN) db-linie 
       (li-create-line bo-create-line sn-create-segment-list))
      
      (3231 "oberird. U-Bahn*" (LI BO SN) db-linie 
       (li-create-line bo-create-line sn-create-segment-list))
      
      (3731 "unterird. U-Bahn*" (LI BO SN) db-linie
       (li-create-line bo-create-line sn-create-segment-list))
      
      ;;; Flaechenbildungskomponenten (SN od. LI) => 
      ;;; evtl. SN-Ketten dekomponieren, spaeter evtl. Strassen rekonstruieren (=> Ketten)
      
      ((3231 2002) "oberird. U-Bahn*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list))
      
      ((3731 2002) "unterird. U-Bahn*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list))
      
      (3162 "Sonstige Strasse*" (LI BO SN) db-line 
       (li-create-line bo-create-line sn-create-segment-list))
      
      (3142 "Hauptverkehrsstrassse*" (LI BO SN) db-line 
       (li-create-line bo-create-line sn-create-segment-list))      
      
      (3112 "BAB*" (LI BO SN) db-line 
       (li-create-line bo-create-line sn-create-segment-list)) ; Randteil der BAB-FL
      
      (3105 "BAB*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list)) ; "unsichtbare" Abgrenzung      
      
      (3152 "BAB-Auffahrt*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list))             
      
      (2002 "Nutzungsartengrenze*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list))
      
      (2212 "Sportplatz*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list))
      
      ((5211 2002) "Bach*" (LI BO SN) db-line 
       (li-create-line bo-create-line sn-create-segment-list))      
      
      (2312 "Schule*" (LI BO SN) db-line
       (li-create-line bo-create-line sn-create-segment-list)) 
      
      (5202 "Teich* (Ufer)" (LI BO SN) db-line 
       (li-create-line bo-create-line sn-create-segment-list)) ; Ufer ?
      
      ;;; Symbole werden teilw. als "interessante" Objekte uminterpretiert
      ;;; um den Datenmangel zu entschaerfen
      
      (8140 "Kirche" SY db-polygon sy-create-church) ; erzeuge ein Kirchen-Polygon 
      (4128 "Baum" SY db-point sy-create-point) ; Wiese/Weide Symbol => 1 Baum
      (3238 "U-Bahn-Station" SY db-point sy-create-point)
      (8113 "Postamt" SY db-polygon sy-create-postoffice)
      (4318 "Moorloch" SY db-polygon sy-create-hole) ; Moor/Sumpf Symbol => 1 Moorloch 
      (4268 "Baum" SY db-point sy-create-point) ; Laubholz Symbol => 1 Baum
      (4218 "Laubbaum" SY db-point sy-create-point)		
      (2238 "Kleingarten" SY db-polygon sy-create-allotment) ; Symbol => 1 Kleingartenhaeusschen
      (8141 "Kapelle" SY db-polygon sy-create-chapel)
      (2258 "Grab" SY db-point sy-create-point) ; Friedhof => 1 Grab
      (8172 "Feuerwehr" SY db-polygon sy-create-firestation)))

;;;
;;;
;;;

(defparameter *os-to-name*
    (make-hash-table :test #'equal))

(defparameter *os-to-item*
    (make-hash-table :test #'equal))

(defparameter *interpreted-os*
    (make-hash-table :test #'equal))


(dolist (item *os*)
  (let ((name (first item))
	(os (second item))
	#| (etyp (third item))
	(tafel (fourth item))
	(ebene (fifth item)) |# )
    (let ((os (if (listp os) os (list os))))
      (setf (gethash os *os-to-name*) name)
      (setf (gethash os *os-to-item*) item))))

(dolist (item *how-to-interpret-os*)
  (let ((os (first item))
	#| (name (second item))
	(sqd-type (third item))		; SQD Element-Typ : listp oder symbolp
	(db-type (fourth item))		; DB-Typ des transformierten Elementes
	(what-to-do (nthcdr 4 item))) |# )
    
    (let ((os (if (listp os) os (list os))))
      (setf (gethash os *interpreted-os*) item))))

(defun lookup-os (os)
  (gethash (if (listp os) os
	     (list os))
	   *os-to-item*))

(defun lookup-interpreted-os (os)
  (gethash (if (listp os) os
	     (list os))
	   *interpreted-os*))

;;;
;;;
;;;

(defun recognized-os-p (os) 
  (lookup-interpreted-os os))


(defun get-db-transform-function (os sqd-type) ; sqd-typ IN { li, sn, pg, } etc. 
  (let* ((item (lookup-interpreted-os os))
	 (possible-sqd-types (third item)))
    (if (consp possible-sqd-types)
	(or (symbol-function
	     (nth (position sqd-type possible-sqd-types)
		  (fifth item)))
	    (error "No db-transform function!"))
      (if (eq sqd-type possible-sqd-types)
	  (symbol-function (fifth item))
	(error "No db-transform function!")))))

#|
(defun get-renamed-os (os)
  (intern (second (lookup-interpreted-os os))))
|#



