;------------------------------------------------------------------------------
; IN : DATA, COUNTS, TEMPS
; OUT: DATA, COUNTS
;------------------------------------------------------------------------------
PRO SUMUP_VARS, means, counts, temps
;------------------------------------------------------------------------------

    wo_ctp = WHERE( (temps.CTP GT 10.) AND (temps.CTH GT 0.)  AND $
                    (temps.CPH GE 0.), nwo_ctp )

    means.CTP[wo_ctp] = means.CTP[wo_ctp] + temps.CTP[wo_ctp]
    means.CTH[wo_ctp] = means.CTH[wo_ctp] + temps.CTH[wo_ctp]
    means.CTT[wo_ctp] = means.CTT[wo_ctp] + temps.CTT[wo_ctp]
    means.CPH[wo_ctp] = means.CPH[wo_ctp] + temps.CPH[wo_ctp]
    counts.CTP[wo_ctp] = counts.CTP[wo_ctp] + 1l

    ; CFC
    wo_cfc = WHERE(temps.CFC GE 0., nwo_cfc)
    means.CFC[wo_cfc] = means.CFC[wo_cfc] + temps.CFC[wo_cfc]
    counts.CFC[wo_cfc] = counts.CFC[wo_cfc] + 1l

    ; daytime CPH
    wo_cph = WHERE(temps.CPH_DAY GE 0., nwo_cph)
    means.CPH_DAY[wo_cph] = means.CPH_DAY[wo_cph] + temps.CPH_DAY[wo_cph]
    counts.CPH_DAY[wo_cph] = counts.CPH_DAY[wo_cph] + 1l

    ; TOTAL CER 
    wo_cer = WHERE(temps.CER GT 0., nwo_cer)
    means.CER[wo_cer] = means.CER[wo_cer] + temps.CER[wo_cer]
    counts.CER[wo_cer] = counts.CER[wo_cer] + 1l

    ; CER_LIQ
    wo_lcer = WHERE(temps.CER_LIQ GT 0., nwo_lcer)
    means.CER_LIQ[wo_lcer] = means.CER_LIQ[wo_lcer] + temps.CER_LIQ[wo_lcer]
    counts.CER_LIQ[wo_lcer] = counts.CER_LIQ[wo_lcer] + 1l

    ; CER_ICE
    wo_icer = WHERE(temps.CER_ICE GT 0., nwo_icer)
    means.CER_ICE[wo_icer] = means.CER_ICE[wo_icer] + temps.CER_ICE[wo_icer]
    counts.CER_ICE[wo_icer] = counts.CER_ICE[wo_icer] + 1l


    ; TOTAL COT 
    wo_cot = WHERE(temps.COT GT 0., nwo_cot)
    means.COT[wo_cot] = means.COT[wo_cot] + temps.COT[wo_cot]
    counts.COT[wo_cot] = counts.COT[wo_cot] + 1l

    ; COT_LIQ
    wo_lcot = WHERE(temps.COT_LIQ GT 0., nwo_lcot)
    means.COT_LIQ[wo_lcot] = means.COT_LIQ[wo_lcot] + temps.COT_LIQ[wo_lcot]
    counts.COT_LIQ[wo_lcot] = counts.COT_LIQ[wo_lcot] + 1l

    ; COT_ICE
    wo_icot = WHERE(temps.COT_ICE GT 0., nwo_icot)
    means.COT_ICE[wo_icot] = means.COT_ICE[wo_icot] + temps.COT_ICE[wo_icot]
    counts.COT_ICE[wo_icot] = counts.COT_ICE[wo_icot] + 1l


    ; TOTAL CWP     
    wo_cwp = WHERE(temps.CWP GT 0., nwo_cwp)
    means.CWP[wo_cwp] = means.CWP[wo_cwp] + temps.CWP[wo_cwp]
    counts.CWP[wo_cwp] = counts.CWP[wo_cwp] + 1l

    ; LWP
    wo_lwp = WHERE(temps.LWP GT 0., nwo_lwp)
    means.LWP[wo_lwp] = means.LWP[wo_lwp] + temps.LWP[wo_lwp]
    counts.LWP[wo_lwp] = counts.LWP[wo_lwp] + 1l

    ; IWP
    wo_iwp = WHERE(temps.IWP GT 0., nwo_iwp)
    means.IWP[wo_iwp] = means.IWP[wo_iwp] + temps.IWP[wo_iwp]
    counts.IWP[wo_iwp] = counts.IWP[wo_iwp] + 1l


    ; TOTAL CWP allsky
    wo_cwp_as = WHERE(temps.CWP_ALLSKY GE 0., nwo_cwp_as)
    means.CWP_ALLSKY[wo_cwp_as] = means.CWP_ALLSKY[wo_cwp_as] $
                                + temps.CWP_ALLSKY[wo_cwp_as]
    counts.CWP_ALLSKY[wo_cwp_as] = counts.CWP_ALLSKY[wo_cwp_as] + 1l

    ; LWP allsky
    wo_lwp_as = WHERE(temps.LWP_ALLSKY GE 0., nwo_lwp_as)
    means.LWP_ALLSKY[wo_lwp_as] = means.LWP_ALLSKY[wo_lwp_as] $
                                + temps.LWP_ALLSKY[wo_lwp_as]
    counts.LWP_ALLSKY[wo_lwp_as] = counts.LWP_ALLSKY[wo_lwp_as] + 1l

    ; IWP allsky
    wo_iwp_as = WHERE(temps.IWP_ALLSKY GE 0., nwo_iwp_as)
    means.IWP_ALLSKY[wo_iwp_as] = means.IWP_ALLSKY[wo_iwp_as] $
                                + temps.IWP_ALLSKY[wo_iwp_as]
    counts.IWP_ALLSKY[wo_iwp_as] = counts.IWP_ALLSKY[wo_iwp_as] + 1l

END
