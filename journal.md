# 27. Februar 2017

Webseite soll neu gestalltet werden.

Anstatt **Yesod** habe ich mich entschlossen [Spock](https://www.spock.li/tutorial/)
zu benutzen, das mir leichtgewichtiger erscheint.

Die Datenbank soll weiterhin auf **PostgreSQL** basieren allerdings möchte ich
einen *EventSourcing* Ansatz verfolgen.

Die Beiträge sollen *gzip*ed per **scp** kopiert und von dort eingefügt werden
können (leichte Übertragung von Bilddateien etc.)

## Beginn der Arbeiten am neuen Auftritt
- Projekt über `stack` mit **Template** `spock` angelegt.
- benutzt *LTS-8.3*
- git Verzeichnis angelegt
- github

## Scaffolding: Middelware(static) / Bootstrap / Lucid
- Statische Inhalte in `./static` über 
[wai-middleware-static](https://hackage.haskell.org/package/wai-middleware-static-0.8.1/docs/Network-Wai-Middleware-Static.html)
einbinden.
- [Lucid](https://github.com/chrisdone/lucid) für das HTML Templating
- [Bootstrap](https://getbootstrap.com/getting-started/) für das Frontend
- Einfaches Templating über `Layout.layout` erstellt

## Layout übernehmen
Das Layout vom aktuellen Blog mittels CSS und `Layout.layout` übernommen


# 28. Februar 2017

## Blog Post rendern
- Rendering zunächst über `highlighting-kate`
- Portiert auf [`skylighting`](https://www.stackage.org/lts-8.3/package/skylighting-0.1.1.5)
(offizieller Nachfolger von `highlighting-kate`)
- `BlogPost` in eigenes Modul
- Layout über `LayoutConfig` konfigurierbar

# 01. März 2017

## Auth
Für den Moment reicht es, wenn ein Administrator über ein Passwort authentifiziert wird.

- Utilities erstellt um Password-Hashes zu schreiben
- Hash für Logon auslesen
- Sessions, State, Logon-Handling in Session-Module ausgelagert

## Spock
- **Parameter** aus dem Body werden mit `param name` oder `params` ausgelesen (nicht dokumentiert!)
- **URL-Decodierung** am Besten mit `Network.HTTP.Types (urlDecode)

## FlyCheck hlint
in Emacs mit

    (flycheck-add-next-checker 'intero '(warning . haskell-hlint))
	
aktiviert (offensichtlich muss mit `stack install hlint` installiert werden)	

# 02. März 2017

## Routen
Die Routen für die Applikation sollen statisch werden und Links entsprechend Typ-Sicher
gerendert werden (siehe [Spock: type-safe-routing](https://www.spock.li/2015/04/19/type-safe_routing.html))

- Routen in `Routes.hs` definiert
- Route über `Request` bestimmen - d.h. die Route soll nicht über die Session
gespeichert werden!

## Layout refaktoriert
die `layout` Funktion bekommt ihre Argumente jetzt aus einem `Page` Record, der
von den einzelnen *Views* bereitgestellt wird

# 21. März 2017
Damit begonnen das Postgres-Datenbank-Backend über `persistent` aufzusetzen.

Die Datenbank ist im Moment fest (siehe `Config.hs`) angelegt.

Einziges Problem war, den `SpockM` Typ richtig zuzordnen (der erste Parameter
ist das **Backend** und ist hier `SqlBackend`) und den SQL-Pool aufzusetzen.

## Context für Admin
Die Technik von [hier](https://www.spock.li/2015/08/23/taking_authentication_to_the_next_level.html)
verwendet um Aktionen (`SiteAdminAction`) die einen Adminzugang voraussetzen
typsicher zu verwenden
