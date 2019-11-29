DEFINE TEMP-TABLE tt-csv NO-UNDO
    FIELD codigo-refer AS CHAR FORMAT "x(20)"
    FIELD rev AS CHAR
    FIELD rev-notes AS CHAR
    FIELD item-desc AS CHAR FORMAT "x(60)"
    FIELD it-codigo AS CHAR
    FIELD date-type AS CHAR
    FIELD date-due  AS CHAR
    FIELD qty-due AS CHAR
    FIELD qty-type AS CHAR
    FIELD cum-qty AS CHAR
    FIELD uom AS CHAR
    FIELD plant AS CHAR FORMAT "x(2)"
    FIELD planner-code AS CHAR
    FIELD supplier AS CHAR 
    FIELD sup-name AS CHAR FORMAT "x(60)"
    FIELD rel AS CHAR
    FIELD rel-date AS CHAR
    FIELD rel-status-code AS CHAR
    FIELD rel-status AS CHAR 
    FIELD balance-out AS CHAR
    FIELD consigment-stock AS CHAR
    FIELD overshipped AS CHAR
    FIELD cum-start-date AS CHAR
    FIELD po AS CHAR
    FIELD po-line AS CHAR
    FIELD ship-code AS CHAR
    FIELD rel-note-1  AS CHAR FORMAT "x(60)"
    FIELD rel-note-2  AS CHAR FORMAT "x(60)"
    FIELD rel-note-3  AS CHAR FORMAT "x(60)"
    FIELD rel-note-4  AS CHAR FORMAT "x(60)"
    FIELD rel-note-5  AS CHAR FORMAT "x(60)"
    FIELD rel-note-6  AS CHAR FORMAT "x(60)"
    FIELD rel-note-7  AS CHAR FORMAT "x(60)"
    FIELD rel-note-8  AS CHAR FORMAT "x(60)"
    FIELD rel-note-9  AS CHAR FORMAT "x(60)"
    FIELD rel-note-10 AS CHAR FORMAT "x(60)"
    FIELD ship-to AS CHAR FORMAT "x(60)"
    FIELD ship-to-addr-1 AS CHAR FORMAT "x(60)"
    FIELD ship-to-addr-2 AS CHAR FORMAT "x(60)"
    FIELD ship-to-addr-3 AS CHAR FORMAT "x(60)"
    FIELD ship-to-addr-4 AS CHAR FORMAT "x(60)"
    FIELD last-rec AS CHAR
    FIELD last-qty AS CHAR
    FIELD last-cum-qty AS CHAR
    FIELD last-pack-nbr AS CHAR
    FIELD new-rev-code AS CHAR
    FIELD new-rev-text AS CHAR
    FIELD new-rev-cum AS CHAR
    FIELD new-rev-start-date AS CHAR
    FIELD amended AS CHAR
    FIELD ship-to-code AS CHAR
    FIELD setup-for-container AS CHAR
    FIELD container-type AS CHAR
    FIELD qty-container AS CHAR
    FIELD schedule-type AS CHAR
    .
