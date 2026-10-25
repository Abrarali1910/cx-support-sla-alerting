%dw 2.0
output application/json

var alerts = payload default []
var fallbackEmail = vars.slaPolicy.notification.fallbackDistributionList

var normalizedAlerts = alerts map (a) -> do {
    var needsFallback =
        a.manager_id == null
        or isEmpty((a.manager_email default "") as String)
        or upper((a.manager_status default "") as String) != "ACTIVE"
    ---
    {
        alert: a,
        recipientKey: if (needsFallback)
            "FALLBACK"
        else
            a.manager_id as String,
        usesFallback: needsFallback
    }
}

---
(normalizedAlerts groupBy ((item) -> item.recipientKey))
    pluck ((items, recipientKey) -> do {
        var managerAlerts = items map ((item) -> item.alert)
        var maxTickets = (vars.slaPolicy.notification.maxTicketsPerDigest default 50) as Number

        var orderedAlerts = managerAlerts orderBy ((a) ->
            if (a.band == "SEVERELY_BREACHED") 0
            else if (a.band == "BREACHED") 1
            else if (a.band == "AT_RISK") 2
            else 3
        )

        var digestAlerts = orderedAlerts filter ((alert, index) -> index < maxTickets)
        var useFallback = items[0].usesFallback
        ---
        {
            managerId: if (useFallback)
                "FALLBACK"
            else
                managerAlerts[0].manager_id,

            managerName: if (useFallback)
                "Support Leadership"
            else
                managerAlerts[0].manager_name,

            managerEmail: if (useFallback)
                fallbackEmail
            else
                managerAlerts[0].manager_email,

            runId: managerAlerts[0].run_id,

            runTime: now() as String {
                format: "yyyy-MM-dd HH:mm:ss"
            },

            escalatedCount: sizeOf(
                (managerAlerts map ((a) -> a.ticket_id))
                    distinctBy ((id) -> id)
            ),

            businessCalendars:
                ((managerAlerts map ((a) -> a.business_calendar_code))
                    distinctBy ((code) -> code))
                    joinBy ", ",

            alertSeqs: managerAlerts map ((a) -> a.alert_seq),

            severeTickets:
    			digestAlerts filter ((a) -> a.band == "SEVERELY_BREACHED"),

			breachedTickets:
    			digestAlerts filter ((a) -> a.band == "BREACHED"),

			atRiskTickets:
    			digestAlerts filter ((a) -> a.band == "AT_RISK"),

			immediateSevereTickets:
    			managerAlerts filter ((a) -> a.band == "SEVERELY_BREACHED"),

			totalAlertCount: sizeOf(managerAlerts),
			displayedAlertCount: sizeOf(digestAlerts)
        }
    })