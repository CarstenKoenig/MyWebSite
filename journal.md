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
