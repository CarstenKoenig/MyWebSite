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
