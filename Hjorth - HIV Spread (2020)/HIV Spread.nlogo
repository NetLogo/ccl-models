
extensions [profiler  nw csv]
directed-link-breed [dummylinks dummylink]
undirected-link-breed [main-links main-link]
undirected-link-breed [casual-links casual-link]
undirected-link-breed [onetime-links onetime-link]

breed [
  M0C0s M0C0
]
breed [
  M0C1s M0C1
]
breed [
  M0C2s M0C2
]
breed [
  M1C0s M1C0
]
breed [
  M1C1s M1C1
]
breed [
  M1C2s M1C2
]

globals [
  file-n ;; this is for running behaviorspace experiments

 hiv-pos-count

 run-number ;; this is to make sure that files from the same run are given an identifier
  alist

  initial-random-seed

  naturally-dying-turtles
  aids-dying-turtles
  deaths-this-week
]

turtles-own [
 hiv-positive?
 diagnosed?
 onetime-quintile
 sexual-role
 age-weeks
 african-american?
 circumcized?
 CCR5-mutation
 viral-load
 infected-at-tick
 hiv-tester?
 last-test-tick
 diagnosed-at-tick
 time-on-ART
 last-treated
 insertivity-preference ;; for versatile people
 full-suppression? ;;      this boolean indicates whether or not this person will achieve full suppression. AH Comment: It's a weird top-down approach IMO and I think we should remove it

 condom-always-pref
 always-condom-casual?
 always-condom-onetime?


 prep-1a ;; These are the prep indications. They each contain a number - the last tick at which this person
 prep-1b ;; would be indicated for prep.
 prep-2a
 prep-2b
 prep-3a
 prep-3b

  ;; these are our experimental prep indications - focusing on whether or nto people have had both insertive and receptive AI within the prep window
  prep-4a ;; this shows whether or not people have had IEV within the window
  prep-4b  ;; this shows whether people have had both receptive and insertive (on different occasions) within the window

 prep-risk-reduction ;; this is assigned "upon birth" and determines their adherence, and consequently the reduction in risk associated with UAI

 last-prep-check

on-prep?

changed-relationship-status?

  ;; these are vairables for importing  from the R code
  traj
  r-stageuse
  stage-time
  in-treat?
  hiv-on-import?
  name
stage

]

links-own [
 hiv-disclosed?
 duration
 intercourses-this-week ;; this is a list of booleans that indicate whether a condom was used or not.
 transmission-this-week
  e_type ;; this is for R-import
  disclosed
  r-hiv-disclosed
  hiv-disclosed
]


to test1 [no-ticks]
  let results (list)
  repeat no-ticks [
    go
    let all-components filter [ c -> count c > 1] nw:weak-component-clusters
    let count-comps length all-components
    let comp-sizes map [ c -> count c] all-components
    let mean-size mean comp-sizes
    let sd-size standard-deviation comp-sizes
    let max-size max comp-sizes
    set results lput (list count-comps mean-size sd-size max-size) results
  ]
  print results
end

to burned-in-setup
  let temp-prep-condition prep-condition
  let temp-n file-n
  ca
  set file-n temp-n
  import-world (word "/burnedin_files/" file-n)
  set prep-condition temp-prep-condition
  update-prep-conditions
end

to setup
  update-prep-conditions
  ca
  reset-ticks
  set initial-random-seed 2147483647 - random 4294967294
  random-seed initial-random-seed
  create-M0C0s n-people [
    initialize-new-person
    ;; They say they create a cross-sectional
    ;; network at first, so I am assuming they create them across all the various ages
    ;; SET AGE TO 18 HERE IF THAT IS WHAT WE END UP DECIDING ON
    ;  set age-weeks 18 * 52
    set age-weeks (18 * 52) + random (52 * 22)
    if infect-on-setup?[
      ifelse age-shifted-prevalence? [
        if random-float 1 < risk-of-initial-infection (25.6 / 29)  and (ccr5-mutation != 2 or ccr5-mutation = 1 and random-float 1 < 30) [
          set hiv-positive? true
          set full-suppression?  random-float 1 < chance-to-be-fully-suppressed? ;; ah comment: this is the odd topdown way of doing this. We can do it for the replication but let's make sure we change it later.

        ]
      ]
      [
        if random-float 1 < .256 [
          set hiv-positive? true
        ]
      ]

      if hiv-positive? [
        set infected-at-tick 0
        if diagnosed-on-setup? and hiv-tester? [
          if random-float 1 < .9 [ ;; although we should end up at .85, this needs to be higher in order to accommodate all the non-HIV testers who have HIV at tick 0
            set diagnosed? true
          ]
        ]
        if treatment-on-setup? and diagnosed? [
          if random-float 1 < .3 [
            set last-treated -1
          ]
        ]
        if distributed-infection? [
          set infected-at-tick random max (list (-1 * (age-weeks - (18 * 52))) -520) ;; adjusted so infection can only occur from age 18 onwards, consistent with dynamics in remainder of model
          let weeks -1 * infected-at-tick
          let tested? false
          while [weeks > 0 and not tested?]
          [
            set tested? getting-tested-this-week?
            set weeks weeks - 1
          ]
          let ever-in-treatment? false
          while [weeks > 0 and not ever-in-treatment?][
            set ever-in-treatment? random-float 1 < 0.1095
            set weeks weeks - 1
          ]
          let currently-in-treatment? true
          while [weeks > 0] [
            set time-on-art time-on-art + 1
            set weeks weeks - 1
            set viral-load calculate-viral-load
            ;; as calculate-viral-load uses the weeks-since-infection (based on infection-at-ticks) which is not updated as the [weeks] value decreases, the viral load obtained at the start is incorrectly calculated
            ifelse currently-in-treatment? [
              if random-float 1 < risk-of-falling-out [
                set currently-in-treatment? false
              ]
            ]
            [
              if random-float 1 <  chance-of-re-achieving-suppression [
                set currently-in-treatment? true
              ]
            ]
          ]
        ]
      ]
    ]
  ]
  set aids-dying-turtles (turtle-set)
  set naturally-dying-turtles (turtle-set)
  reset-ticks
end

to-report vars-to-list [list-of-varnames]
  report map [ var-name -> (list var-name run-result var-name)] list-of-varnames

end

to-report risk-of-initial-infection [scale]
 let temp age-years
 (ifelse
 temp = 18 [report 0.005964215 * scale]
 temp = 19 [report 0.032818533 * scale]
 temp = 20 [report 0.046966732 * scale]
 temp = 21 [report 0.1	* scale]
 temp = 22 [report 0.104208417 * scale]
 temp = 23 [report 0.160583942 * scale]
 temp = 24 [report 0.207089552 * scale]
 temp = 25 [report 0.235294118 * scale]
 temp = 26 [report 0.263366337 * scale]
 temp = 27 [report 0.265873016 * scale]
 temp = 28 [report 0.329268293 * scale]
 temp = 29 [report 0.366666667 * scale]
 temp = 30 [report 0.348336595 * scale]
 temp = 31 [report 0.39453125 * scale]
 temp = 32 [report 0.432432432 * scale]
 temp = 33 [report 0.415966387 * scale]
 temp = 34 [report 0.4 * scale]
 temp = 35 [report 0.48173516 * scale]
 temp = 36 [report 0.474157303 * scale]
 temp = 37 [report 0.449771689 * scale]
 temp = 38 [report 0.43559719 * scale]
 temp = 39 [report 0.474056604 * scale]
 [report 0]
  )

end

to-report sr-reporter
  let srtemp random 1000
  (ifelse
    srtemp < 286 [report "insertive"]
    srtemp < 611 [report "receptive"]
    [report "versatile"]
  )
end

to-report CCR5-reporter
  let CCRtemp random 1000
  (ifelse
    CCRtemp < 17 [report 2]
    CCRtemp < 116 [report 1]
    [report 0]
  )
end

to-report ACU-reporter
  let ACUtemp random 10000
  (ifelse
    ACUtemp < 6185 [report 0]
    ACUtemp < 7807 [report 1]
    ACUtemp < 9497 [report 2]
    [report 3]
  )
end

to-report PRR-reporter
  let PRRtemp random 1000
  (ifelse
    PRRtemp < 221 [report 0.00]
    PRRtemp < 281 [report 0.31]
    PRRtemp < 381 [report 0.81]
    [report 0.95]
  )
end


to initialize-new-person
  set hiv-positive? false
  set diagnosed? false
  set onetime-quintile who mod 5
  set sexual-role sr-reporter
  set CCR5-mutation CCR5-reporter
  set african-american? one-of [true false]
  set circumcized? ifelse-value (random 1000 < 896) [true] [false]
  set age-weeks 18 * 52
  set hiv-tester? random 1000 > 65
  set last-test-tick 0
  set last-treated -10000
  set time-on-ART 0
  set insertivity-preference random-float 1
  set condom-always-pref ACU-reporter
  if condom-always-pref = 0 [
    set always-condom-casual? FALSE
    set always-condom-onetime? FALSE]
  if condom-always-pref = 1 [
    set always-condom-casual? TRUE
    set always-condom-onetime? FALSE]
  if condom-always-pref = 2 [
    set always-condom-casual? FALSE
    set always-condom-onetime? TRUE]
  if condom-always-pref = 3 [
    set always-condom-casual? TRUE
    set always-condom-onetime? TRUE]
  set prep-1a  -10000
  set prep-1b -10000
  set prep-2a -10000
  set prep-2b -10000
  set prep-3a -10000
  set prep-3b  -10000
  set on-prep? false
  set last-prep-check -10000
  set prep-risk-reduction PRR-reporter
  set changed-relationship-status? false
  set full-suppression?  random-float 1 < chance-to-be-fully-suppressed? ;; ah comment: this is the odd topdown way of doing this. We can do it for the replication but let's make sure we change it later.
end

to go
  if ticks > 30000 [stop]
  ;; think about order here, it's really important. I think we do:
  ;; sex, age, breakup, die, add new people, create-network
  ;; We want the create-network to be the last thing that happens so that we can get the best idea of
  ;; what the network looks like. However: that also means that the measurements we get at the end of the tick
  ;; on our plots might not be a good reflection of what happened during this tick... maybe move it up? Is
  ;; there a reason not to?


  ask links [
    set intercourses-this-week (list)
    set duration duration - 1
    set transmission-this-week false
  ]
  ask turtles [set changed-relationship-status? false]
  ask links with [duration <= 0] [
    ask both-ends [set changed-relationship-status? true] ;; this is ugly but most optimal
    die ] ;; some relationships break up
  ask turtles [set breed appropriate-breed] ;; need to update breed because we just changed links.
  create-m0c0s random-poisson (0.001 * n-people) [ ;; rate is hard coded
      initialize-new-person
    ]

  update-constrained-network ;; and new relationships are formed


  ;; all relationships have AI some number of times
  ask links [
    set intercourses-this-week map [ [ ] -> using-condom?] range calculate-intercourses-this-week  ;; this gives us a list of bools for whether or not they used condom

    ;; Now we find out whether these intercourses comply with prep indications
    ;; indcation 1a: UAI in double sided monogamous negative partnership with partner not recetly tested negative for HIV
    if [diagnosed?] of both-ends = [false false] and [count my-links] of both-ends = [1 1] and length UAIs-this-week > 0 [
      if [ticks - last-test-tick < partner-testing-window-ind1] of end1 [
        ask end2 [set prep-1a ticks]
      ]
      if [ticks - last-test-tick < partner-testing-window-ind1] of end2 [
        ask end1 [set prep-1a ticks]
      ]
    ]

    ;; Now we find out whether these intercourses comply with prep indications
    ;; indcation 1b: UAI in a single sided monogamous negative partnership with partner not recetly tested negative for HIV
    if [diagnosed?] of both-ends = [false false]  and length UAIs-this-week > 0  [
      if [ticks - last-test-tick < partner-testing-window-ind1] of end1 and [count my-links = 1] of end2 [ ;; if end 2 is monogamous and end1 hasn't been tested within the window
        ask end2 [set prep-1b ticks] ;; end2 can be indicated for prep at this time
      ]
      if [ticks - last-test-tick < partner-testing-window-ind1] of end2 and [count my-links = 1] of end1 [
        ask end1 [set prep-1b ticks]
      ]
    ]

    ;; indication 2a & 2b are calculated after all intercourses are done:
    ;; Indication 3a: AI within KNOWN serodiscordant partnerships of type main or casual:
    if (is-main-link? self or is-casual-link? self) and hiv-disclosed? and count both-ends with [diagnosed?] = 1 and length intercourses-this-week > 0 [
      ask both-ends with [not diagnosed?] [set prep-3a ticks]
    ]

    ;; Indication 3b: Unprotected AI within serodiscordant partnership  of type main or casual:
    if (is-main-link? self or is-casual-link? self) and hiv-disclosed? and count both-ends with [diagnosed?] = 1 and length UAIs-this-week > 0 [
      ask both-ends with [not diagnosed?] [set prep-3b ticks]
    ]


    ;; now we calculate the risks of HIV transmission. We only do this for serodiscordant couples
    if count both-ends with [hiv-positive?] = 1  [

      ;; we create a list with booleans for each intercourse, seeing if the hiv- was infected
      let infection-bool-list map [ [ condom? ] -> random-float 1 < risk-of-transmission-factors condom?] intercourses-this-week
      ;; filter it and see how long it is. I'm sure there's a reduce or sentence for this.
      if length filter [[infected?] -> infected?] infection-bool-list > 0 [ ;;
        set transmission-this-week true
        ask hiv- [
          set hiv-pos-count hiv-pos-count + 1
          set hiv-positive? true set infected-at-tick ticks]
      ]
    ]
  ]

  ask turtles with [count my-links > 0 and not hiv-positive?] [
    ;; this is a list of lists of booleans.
    let my-intercourses-this-week [intercourses-this-week] of my-links
    ;; if there is more than one list that contains false, they had UAI in more than one of their relationships
    if length filter [[intercourses] -> member? false intercourses] my-intercourses-this-week > 1 [set prep-2a ticks]
    ;; do this again, but only for casual and one-time
    set my-intercourses-this-week [intercourses-this-week] of my-links with [is-casual-link? self or is-onetime-link? self]
    ;; if there is more than one list that contains false, they had UAI in more than one of their relationships
    if length filter [[intercourses] -> member? false intercourses] my-intercourses-this-week > 0 [set prep-2b ticks]
  ]


;; maybe go off prep
  ask turtles with [on-prep? and (ticks - last-prep-check) >= 52] [ ;;; AH: THIS WAS WRONG. I re-indicated people every tick. Now I only do it once every year.
    ;; re-indicate here. If they are not indicated, take them off prep
    ;; in other words, default is not to be on prep, so:
    set on-prep? false
    if indicate-1a and ticks - prep-1a < indication-window [ set on-prep? true  set last-prep-check ticks ]
    if indicate-1b and ticks - prep-1b < indication-window [ set on-prep? true set last-prep-check ticks  ]
    if indicate-2a and ticks - prep-2a < indication-window [ set on-prep? true set last-prep-check ticks  ]
    if indicate-2b and ticks - prep-2b < indication-window [ set on-prep? true  set last-prep-check ticks ]
    if indicate-3a and ticks - prep-3a < indication-window [ set on-prep? true  set last-prep-check ticks ]
    if indicate-3b and ticks - prep-3b < indication-window [ set on-prep? true  set last-prep-check ticks  ]

  ]

;; maybe go in for HIV testing.
;; this should come before aging, so that the first week "counts" towarsd
;; the three week rule of testing (and viral load)
let this-weeks-hiv-testers turtles with  [hiv-tester? and not diagnosed? and getting-tested-this-week?]
;show (word count this-weeks-hiv-testers " were tested this week")
ask this-weeks-hiv-testers [
  set diagnosed? test-results-positive?
  set last-test-tick ticks
  if diagnosed? [ ;; here is where people get diagnosed as HIV positive
    set diagnosed-at-tick ticks
    ask (link-set my-main-links my-casual-links) [set hiv-disclosed? true] ;; always disclose HIV+ status if in relationship at time of diagnosis
  ]
;; if we are doing prep
  if prep? [
  ;; and if there is room for more people on prep
    if not diagnosed? and not on-prep? [
      if count turtles with [on-prep?] < prep-coverage-fraction * count turtles [ ;; this is the last checkwe should make because it's the most resource intensive
                                                                                  ;; optimize this by moving the 'prep?' check one level up ;; AH: DONE
        if indicate-1a and ticks - prep-1a < indication-window [ set on-prep? true  set last-prep-check ticks ]
        if indicate-1b and ticks - prep-1b < indication-window [ set on-prep? true set last-prep-check ticks  ]
        if indicate-2a and ticks - prep-2a < indication-window [ set on-prep? true set last-prep-check ticks  ]
        if indicate-2b and ticks - prep-2b < indication-window [ set on-prep? true  set last-prep-check ticks ]
        if indicate-3a and ticks - prep-3a < indication-window [ set on-prep? true  set last-prep-check ticks ]
        if indicate-3b and ticks - prep-3b < indication-window [ set on-prep? true  set last-prep-check ticks  ]
          if indicate-4a and sexual-role = "versatile" [set on-prep? true set last-prep-check ticks]
      ]
    ]
  ]
]

;; maybe or leave treatment
;; Need to break this up into two steps so we first get the ones that are currently in treatment, and the ones that
;; are not, so we we don't have some people enter and leave treatment in the same tick
let turtles-in-treatment-last-week turtles with [last-treated = ticks - 1]
let fallen-out-turtles turtles with [diagnosed? and ever-treated? and not in-treatment?]
let untreated-but-diagnosed-turtles turtles with [diagnosed? and not ever-treated?]

;; turtles could drop out
ask turtles-in-treatment-last-week [
  ;; we model treatment by setting the last treatment time to the current tick
  if random-float 1 > risk-of-falling-out [
    set last-treated ticks
  ]
];; they could come back
ask fallen-out-turtles [
  if random-float 1 < chance-of-re-achieving-suppression [
    set last-treated ticks
  ]
]
;; finally they can start for the first time if they are diagnosed
ask untreated-but-diagnosed-turtles  [
  if random-float 1 < 0.1095 [
      set last-treated ticks
    ]
  ]

  ;; Turtles age and potentially die
  ask turtles [
    set age-weeks age-weeks + 1
    if in-treatment? [set time-on-ART time-on-ART + 1]
    if hiv-positive?[
      set viral-load calculate-viral-load
    ]
  ]

  set naturally-dying-turtles turtles with [random-float 1 < natural-mortality-rate]
  set aids-dying-turtles turtles with [viral-load >= 7]
  set deaths-this-week count (turtle-set  naturally-dying-turtles  aids-dying-turtles)



;  show count aids-dying-turtles
  ask naturally-dying-turtles [die]
  ask aids-dying-turtles [die]


;  if sexual-activity-network? [ ;; we add at what points in time two people haev been in a relationship & how many intercourses they had that week
;;    ask links [
;    ask onetime-links  [
;      ls:let end-1 [who] of end1
;      ls:let end-2 [who] of end2
;      ls:let count-this-weeks-intercourses length intercourses-this-week
;      ls:ask 1 [
;        ask turtle end-1 [create-link-with turtle end-2]
;        ask link end-1 end-2 [
;          if ticks-in-relationship  = 0 [set ticks-in-relationship  (list)]
;          if intercourses-at-tick  = 0 [set intercourses-at-tick  (list)]
;        set ticks-in-relationship lput ticks ticks-in-relationship
;        set intercourses-at-tick lput count-this-weeks-intercourses  intercourses-at-tick
;        ]
;      ]
;    ]
;  ]

  show count turtles with [on-prep?]


;  show count links with [transmission-this-week]

;  ls:ask ls:models [tick]
  tick

  ;; every half year we export the world
;if ticks mod 104 = 0 [export-world (word "/ThisFolderWillBe12GB_unsync_at_will/worldExport_ " run-number "_atTick_" ticks)]
;  if ticks = 520 [stop]

end

to update-constrained-network
  ;; first create mains and casuals. Onetime sex depends on people's main and casual status, so we need to determine that first
  let potential-mains eligible-main-turtles
  let potential-casuals eligible-casual-turtles
  let done? false
  while [not done?] [ ;; turn this into a recursive function at some point, so we don't have to do this while-stuff
    if count potential-mains > 2 [
      ask one-of potential-mains [
        ;; filter out sexually incompatible first
        let my-potential-mains other potential-mains with [sexually-compatible? and not member? myself link-neighbors]
        ;; get only the ones of the age that this turtle wants now
        let potential-partners age-appropriate-partners my-potential-mains 0.464
        let new-partner one-of potential-partners
        create-main-link-with new-partner [begin-relationship];; this is where we can add a utility function later
        ask new-partner [set breed appropriate-breed]
        set breed appropriate-breed
      ]
    ]
    if count potential-casuals > 2 [
      ask one-of potential-casuals [
        let my-potential-casuals other potential-casuals with [sexually-compatible? and not member? myself link-neighbors]
        let potential-partners age-appropriate-partners my-potential-casuals 0.586        let new-partner one-of potential-partners
        create-casual-link-with new-partner [begin-relationship] ;; add utility/attraction function
        ask new-partner [set breed appropriate-breed]
        set breed appropriate-breed
      ]
    ]

    set potential-mains eligible-main-turtles
    set potential-casuals eligible-casual-turtles
    if count potential-mains < 2 and count potential-casuals < 2 [
      set done? true
    ]
  ]
  let onetime-seekers turtles with [random-float 1 < probability-of-onetime-ai]
  while [count  onetime-seekers > 2] [
    ask one-of onetime-seekers [
      let my-potential-onetimes age-appropriate-partners (other onetime-seekers with [sexually-compatible? and not member? myself link-neighbors]) 0.544
      ;; it is possible that there are no potential onetime partners because of sexual role compatibility, so if that is
      ;; the case, we just remove this person from onetime-seekers
      ifelse count my-potential-onetimes = 0
      [
        set onetime-seekers other onetime-seekers
      ]
      [
        ;; if there is still no potential partners, it is because we got an uneven number of
        ;; people with compatible sexual roles, so in that case we just quit ou
        create-onetime-link-with one-of my-potential-onetimes [begin-relationship]
        set onetime-seekers onetime-seekers with [not any? my-onetime-links]
      ]
    ]
  ]
end

to-report using-condom?
  report random-float 1 < probability-of-using-condom
end

to-report chance-to-be-fully-suppressed?
  report ifelse-value african-american? [0.614] [0.651]
end

;; We model this as being averaged out across the two populations,
;; but if we want to distinguish between them, use the "days-between-testing"
;; reporter, which reports different numbers by race
to-report getting-tested-this-week?
  ;; there are seven days in a week so we "try" in 7 times
  repeat 7 [
    if random 308 < 1 [
      report true
    ]
  ]
  report false
end

to-report days-between-testing
  ifelse african-american? [
    report 301
  ]
  [
    report 315
  ]
end

to-report test-results-positive?
    ;; the supplementary materials say that certain things were recorded for turtles
    ;; who are diagnosed, but I'm not sure which ones. This is where it would go though.
  report weeks-since-infected > 3 and hiv-positive?
end

to-report calculate-intercourses-this-week
    if is-main-link? self [
    report random-poisson (1.54 * AI_scale)
  ]
  if is-casual-link? self [
    report random-poisson (0.96 * AI_scale)
  ]
  if is-onetime-link? self [
    report random-poisson (1 * AI_scale)
  ]
end

to begin-relationship
  set hiv-disclosed? false ;; default is that it is not disclosed
  if count both-ends with [hiv-positive? and diagnosed?] > 0;;AH question: should this not be a reciprocal process? if two people are HIV+, isn't it more likely that one of them say something?
  [
    set hiv-disclosed? disclose-or-not?
  ]
  set duration randomized-relationship-duration
  set transmission-this-week false
end

to-report is-serodiscordant? ;; link procedure, reports yes if one person is HIV+
  report count both-ends with [hiv-positive?] = 1
end


;; given two coefficients, we calculate the probability that the couple uses a condom
;; link procedure
;to-report probability-of-using-condom
;  let log-odds relationship-condom-use-coefficient + condition-condom-use-coefficient
;  report log-odds-to-prob log-odds
;end

to-report probability-of-using-condom
  let log-odds relationship-condom-use-coefficient
  if breed = main-links [report log-odds-to-prob log-odds]
  if breed = casual-links [ifelse any? both-ends with [always-condom-casual?] [report 1][report log-odds-to-prob log-odds]]
  if breed = onetime-links [ifelse any? both-ends with [always-condom-onetime?] [report 1][report log-odds-to-prob log-odds]]
end

;; link procedure
to-report relationship-condom-use-coefficient
  let Unprotected_AI 0
  if is-main-link? self [ ;; per relationship type get the likelihood of NOT using a condom
    set Unprotected_AI 1 - 0.21 ] ;; the way the documentation mentions it is the logodds of -1.325 which is not strictly the same...
  if is-casual-link? self [
    set Unprotected_AI 1 - 0.26 ] ;; the way the model implementes the logodds of -1.046 which is not strictly the same...
  if is-onetime-link? self [
    set Unprotected_AI 1 - 0.27 ] ;; the way the model implementes the logodds of -1.008 which is not strictly the same...
  ;; change to log-odds
  let logodds_UAI ln (Unprotected_AI / (1 - Unprotected_AI )) ;;AH: I'm not sure how to interpret this. For main links, it means that it
  ;; is .79 / .21. Is that right? Can that be interpreted in any meaningful way? And is it definitely turned to log odds first, and then
  ;; we deal wiith disclosed and diagnosed?
  ;; if the status is disclosed the log-odds are decreased a bit
  if hiv-disclosed? [ ;; and if it's disclosed, then even more likely
    set logodds_UAI logodds_UAI - .850
  ]
  ;; if the the hiv+ person is diagnosed the log-odds decreased a bit
  if any? both-ends with [diagnosed?] [;; shouldn't it make it more likely if BOTH ends are, even if they don't disclose it? WV: if both are diagnosed the link is not serodiscordant anymore
    set logodds_UAI logodds_UAI - .670
  ]
  let protected_AI logodds_UAI * -1
  report protected_AI
end

;;; link procedure
;to-report relationship-condom-use-coefficient
;  if is-main-link? self [
;    report ln (0.21 / (1 - 0.21 )) ;; the way the model implementes the logodds of -1.325 which is not strictly the same...
;  ]
;  if is-casual-link? self [
;    report ln (0.26 / (1 - 0.26))   ;; the way the model implementes the logodds of -1.046 which is not strictly the same...
;  ]
;  if is-onetime-link? self [
;    report ln (0.27 / (1 - 0.27)) ;; the way the model implementes the logodds of -1.008 which is not strictly the same...
;  ]
;end

;; link procedure
to-report condition-condom-use-coefficient
  if not any? both-ends with [diagnosed?] [report 0] ;; if neither is diagnosed, this doesn't matter
  if hiv-disclosed? [ ;; and if it's disclosed, then even more likely
    report 0.850
  ]
if any? both-ends with [diagnosed?] [ ;;But shouldn't it make it more likely if BOTH ends are, even if they don't disclose it? WV: if both are diagnosed its not serodiscordant anymore so it does not matter...
    report 0.670
  ]
  ;  report 0 ;; commenting this out. It should never get to here.
end

;;; a poisson distribution with a mean of 3 per 10,000 people
;;; Importantly, this will not really work for smaller populations because
;;; we'll just keep creating small numbers of people
;to-report new-people-this-week
;  ;; first we add to our global
;  set people-fractions-to-add people-fractions-to-add + random-poisson 3 * count turtles / 10000
;  let people-whole floor people-fractions-to-add
;  set people-fractions-to-add remainder people-fractions-to-add 1
;  report people-whole
;end



;; Network construction helper procedures
;; the network must always have this distribution
;; we should never go under this

;           0 Casual 1 Casual 2 Casual
;  0 Main   47.1%    16.7%    7.4%
;  1 Main   22.0%    4.7%     2.1%

;;; this takes a turtleset and returns all those turtles that are eligible for
;;; a main link. This depends on two things: first, they cannot already have
;;; a main link, and second, there cannot already be too many in the cell that
;;; they would move into if they got a main link
;to-report eligible-main-turtles [t-set]
;  let eligible-turtles (turtle-set) ;; only ever ones that don't have one already
;  ;; add each of the three if they are OK
;  set eligible-turtles  ifelse-value (not enough-m1-c0s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 0 and count my-casual-links = 0])] [eligible-turtles]
;  set eligible-turtles  ifelse-value (not enough-m1-c1s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 0 and count my-casual-links = 1])] [eligible-turtles]
;  set eligible-turtles  ifelse-value (not enough-m1-c2s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 0 and count my-casual-links = 2])] [eligible-turtles]
;  report eligible-turtles
;end
;
;;; this takes a turtleset and returns those turtles that are eligible for
;;; a casual link. They must have less than 2 casual links already, and
;;; depending on the state of the rest of the network, we design an appropriate
;;; turtleset
;to-report eligible-casual-turtles [t-set]
;  let eligible-turtles (turtle-set)
;  set eligible-turtles ifelse-value (not enough-m0-c2s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 0 and count my-casual-links = 1])] [eligible-turtles]
;  set eligible-turtles ifelse-value (not enough-m0-c1s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 0 and count my-casual-links = 0])] [eligible-turtles]
;  set eligible-turtles ifelse-value (not enough-m1-c2s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 1 and count my-casual-links = 1])] [eligible-turtles]
;  set eligible-turtles ifelse-value (not enough-m1-c1s?) [(turtle-set eligible-turtles t-set with [count my-main-links = 1 and count my-casual-links = 0])] [eligible-turtles]
;  report eligible-turtles
;end
;


;; this takes a turtleset and returns all those turtles that are eligible for
;; a main link. This depends on two things: first, they cannot already have
;; a main link, and second, there cannot already be too many in the cell that
;; they would move into if they got a main link
to-report eligible-main-turtles
  let eligible-turtles (turtle-set) ;; only ever ones that don't have one already
  ;; add each of the three if they are OK
  set eligible-turtles  ifelse-value (not enough-m1-c0s?) [(turtle-set eligible-turtles m0c0s )] [eligible-turtles]
  set eligible-turtles  ifelse-value (not enough-m1-c1s?) [(turtle-set eligible-turtles m0c1s )] [eligible-turtles]
  set eligible-turtles  ifelse-value (not enough-m1-c2s?) [(turtle-set eligible-turtles m0c2s )] [eligible-turtles]
  report eligible-turtles
end

;; this takes a turtleset and returns those turtles that are eligible for
;; a casual link. They must have less than 2 casual links already, and
;; depending on the state of the rest of the network, we design an appropriate
;; turtleset
to-report eligible-casual-turtles
  let eligible-turtles (turtle-set)
  set eligible-turtles ifelse-value (not enough-m0-c2s?) [(turtle-set eligible-turtles m0c1s )] [eligible-turtles]
  set eligible-turtles ifelse-value (not enough-m0-c1s?) [(turtle-set eligible-turtles m0c0s )] [eligible-turtles]
  set eligible-turtles ifelse-value (not enough-m1-c2s?) [(turtle-set eligible-turtles m1c1s )] [eligible-turtles]
  set eligible-turtles ifelse-value (not enough-m1-c1s?) [(turtle-set eligible-turtles m1c1s )] [eligible-turtles]
  report eligible-turtles
end







;to-report m0-c0s
;  report turtles with [ count my-main-links = 0 and count my-casual-links = 0]
;end
;to-report m0-c1s
;  report turtles with [ count my-main-links = 0 and count my-casual-links = 1]
;end
;to-report m0-c2s
;  report turtles with [ count my-main-links = 0 and count my-casual-links = 2]
;end
;to-report m1-c0s
;  report turtles with [ count my-main-links = 1 and count my-casual-links = 0]
;end
;to-report m1-c1s
;  report turtles with [ count my-main-links = 1 and count my-casual-links = 1]
;end
;to-report m1-c2s
;  report turtles with [ count my-main-links = 1 and count my-casual-links = 2]
;end

to-report target-size-m0-c0s
  report round count turtles * .471
end
to-report target-size-m0-c1s
  report round count turtles * 0.167
end
to-report target-size-m0-c2s
  report round count turtles * 0.074
end
to-report target-size-m1-c0s
  report round count turtles * 0.22
end
to-report target-size-m1-c1s
  report round count turtles * 0.047
end
to-report target-size-m1-c2s
  report round count turtles * 0.021
end
to-report enough-m0-c1s?
  report (count m0c1s >= target-size-m0-c1s)
end
to-report enough-m0-c2s?
  report (count m0c2s >= target-size-m0-c2s)
end
to-report enough-m1-c0s?
  report (count m1c0s >= target-size-m1-c0s)
end
to-report enough-m1-c1s?
  report (count m1c1s >= target-size-m1-c1s)
end
to-report enough-m1-c2s?
  report (count m1c2s >= target-size-m1-c2s)
end


;; AH question: I am not sure this is correctly interpreted. Do we take the sum of them,
;; or the product of them? What do they mean?

;; AH question: related, is this the chance that someone will have a onetime AI during one week
;; or is it the average onetime AIs they have per week (i.e. not a binary distribution)

;; AH question: also, what the hell is a time step? they don't say that anywhere. Is it definitely a week?
;; AH reply: It seems like it? THey don't say it outright, but it *feels* like they use them interchangably

;; AH comment: this implementation gives an average chance across the entire population
;; of a random AI encounter of 15%. That seems immediately reasonable, but I could
;; be way off. At least it doesn't seem entirely crazy
to-report probability-of-onetime-ai
  let rate (onetime-ai-quintile-effect * onetime-ai-relationship-effect)
  report rate

end

to-report get-quintile

end


to-report onetime-ai-quintile-effect
  (ifelse
    onetime-quintile = 0[report 0 / 0.0674]
    onetime-quintile = 1[report 0.007 / 0.0674]
    onetime-quintile = 2[report 0.038 / 0.0674]
    onetime-quintile = 3[report 0.071 / 0.0674]
    onetime-quintile = 4[report 0.221 / 0.0674]
    [report false]
  )
end
; 0 Casual 1 Casual 2 Casual
; 0 Main 0.065 0.087 0.086
; 1 Main 0.056 0.055 0.055
to-report onetime-ai-relationship-effect
  let cnt-links (list count my-main-links count my-casual-links)
  (ifelse
     cnt-links = [0 0][report 0.065]
     cnt-links = [0 1][report 0.087]
     cnt-links = [0 2][report 0.086]
     cnt-links = [1 0][report 0.056]
     cnt-links = [1 1][report 0.055]
     cnt-links = [1 2][report 0.055]
    [report false]
    )
end



;; Relationship-building helper procedures

;; turtle procedure, returns a score that we can use as a weight
;; for randomizing the selection of new parter



;; and for gods sake, make sure this is symmetric
to-report preference-score [potential-partner age-mean-value]
  ;; Currently the only thing we know from the Jenness paper is that people have a
  ;; preference relating to their age and the age of the other person
  ;; the mean value depends on the type of relationship, so we pass
  ;; that in as an argument


end
to-report randomized-relationship-duration
  if is-main-link? self [
    report random-exponential 58.1
  ]
  if is-casual-link? self [
    report random-exponential 23.71
  ]
  if is-onetime-link? self [
    report 1
  ]
end
to-report disclose-or-not?
  if is-main-link? self [
    report random 1000 < 787
  ]
  if is-casual-link? self [
    report random 1000 < 678
  ]
  if is-onetime-link? self [
    report random  1000 < 568
  ]
end


;; The jenness paper doesn't say anything about distribution in age-mixing,
;; only the average score. I think it's reasonable to assume a normal distribution
;; for sexual partners, but it's sort of a weird way of scoring it.
;; But whatever, this should sort of work. We can rewrite it to be a random distribution
;; instead later.
;; So, we first get the score that we want, centered around the
;; mean value that we want. I have no idea what the SD would or could be, but
;; preferrably as permissive as possible while at the same time maintaining the right
;; mean value across the population. Let's try with one?
;; My first intuition was that we would want the SD to vary with age, but
;; the age-mixing-score actually takes that into account for us, so let's try with
;; 1 for everybody. (Keep in mind, this is the SCORE, not the age, so it's not a
;; SD of .1 for age difference.)

;; nope, .1


;; Turtle procedure, takes a set of potential partners and a desired age mix score
;; mean. Find a number that is normally distributed around that desired mean
;; and return partners with the cloest fit to that age mix score
to-report age-appropriate-partners [potential-partners desired-age-score-mean]
  let desired-age-mix-score random-normal desired-age-score-mean .1 ;; turns out .1 is a good number. Who knew.
  let pot-partners potential-partners with-min [abs (turtle-age-mix-score myself - desired-age-mix-score)]
  report pot-partners
end

;; link procedure, gives us age mixing score
to-report age-mixing-score
  report abs (sqrt [age-years] of end1 - sqrt [age-years] of end2)
end
;; turtle procedure, takes another turtle, returns age mix score
to-report turtle-age-mix-score [another-turtle]
  report abs (sqrt age-years - sqrt [age-years] of another-turtle)
end



;; turtle helper procedures
to-report age-years
  report floor (age-weeks / 52)
end


;; turtle context, asked by another turtle
to-report sexually-compatible?
  report sexual-role = "versatile" and [sexual-role] of myself = "versatile" or sexual-role != [sexual-role] of myself
end

;; link reporter
to-report age-difference
  report abs ([age-years] of end1 - [age-years] of end2)
end

to-report natural-mortality-rate
  (ifelse
  not african-american? [
    (ifelse
        age-years <= 24 [report (1.00103 ^ (1 / 52)) - 1]
        age-years <= 34 [report (1.00133  ^ (1 / 52)) - 1]
        age-years <= 39 [report (1.00214 ^ (1 / 52)) - 1]
        [report 1]
    )
  ]
  african-american?[
    (ifelse
        age-years <= 24 [report (1.00159  ^ (1 / 52)) - 1]
        age-years <= 34 [report (1.00225 ^ (1 / 52)) - 1]
        age-years <= 39 [report (1.00348 ^ (1 / 52)) - 1]
        [report 1]
     )
  ]
  [report 0]
    )
end

to-report calculate-viral-load
  ifelse time-on-ART = 0 [
    report calculate-untreated-viral-load][
    ifelse weeks-since-infected < (52 * 10) [
      report calculate-treated-viral-load
    ][
      report calculate-aids-viral-load
    ]
  ]
end

to-report calculate-untreated-viral-load
  report untreated-viral-load
end

to-report calculate-treated-viral-load
  let stage1to3-endpoint 4.5
  ifelse in-treatment? [
    report max (list (viral-load - 0.25) minimum-viral-load) ]
  [
    report min (list (viral-load + 0.25) stage1to3-endpoint )
  ]
end

to-report calculate-aids-viral-load
  let vl-fatal 7
  let vl-setpoint 4.5
  let AIDS-durations 52 * 2
  let AIDS? in-AIDS-stage?
  let AIDS-increase (vl-fatal - vl-setpoint) / AIDS-durations

  ifelse AIDS? [
    report viral-load + AIDS-increase
  ][
    ifelse in-treatment? [
      report max (list (viral-load - 0.25) minimum-viral-load) ][
      report min (list (viral-load + 0.25) vl-setpoint)
    ]
  ]
end

to-report in-AIDS-stage?
  let time-off-ART weeks-since-infected - time-on-ART
  let in-AIDS-stage FALSE
  let max-time-on-ART 52 * 15
  let max-time-off-ART 52 * 10
  if last-treated < 0 and weeks-since-infected > 520 [ set in-AIDS-stage TRUE]
  ifelse full-suppression? [
    if time-on-ART >= max-time-on-ART [ set in-AIDS-stage TRUE ]
  ][
    if ((time-on-ART / max-time-on-ART) + (time-off-ART / max-time-off-ART)) > 1 [set in-AIDS-stage TRUE ]
  ]
  report in-AIDS-stage
end

to-report minimum-viral-load
  report ifelse-value full-suppression? [1.5] [3.5]
end

to-report untreated-viral-load
  let the-stage 0
  let stage1-duration 6
  let stage2-duration 6
  let stage3-duration ( 10 * 52 ) - stage1-duration - stage2-duration
  let stage4-duration 104
  let stage1-endpoint 6.886
  let stage2-endpoint 4.5
  let stage3-endpoint 4.5
  let stage4-endpoint 7
  let d1 stage1-endpoint / stage1-duration
  let d2 (stage2-endpoint - stage1-endpoint) / stage2-duration
  let d3 (stage3-endpoint - stage2-endpoint) / stage3-duration
  let d4 (stage4-endpoint - stage4-endpoint) / stage4-duration

  if weeks-since-infected <= stage1-duration [ ;; we're in stage 1
    let weeks-into-current-stage weeks-since-infected
    report (weeks-into-current-stage * d1)
  ]
  if weeks-since-infected <= stage1-duration + stage2-duration [ ;; we're in stage 2
    let weeks-into-current-stage weeks-since-infected - stage1-duration
    report stage1-endpoint + (d2 * weeks-into-current-stage)
  ]
  if weeks-since-infected <= (stage1-duration + stage2-duration + stage3-duration) [ ;; we're in stage 3
    report stage3-endpoint ;; this is a flatline so any time in stage 3 is 10 ^ 4.5
  ]
  if weeks-since-infected < (stage1-duration + stage2-duration + stage3-duration + stage4-duration) [ ;; we're in stage 4
    let weeks-into-current-stage weeks-since-infected - (stage1-duration + stage2-duration + stage3-duration)
    report stage3-endpoint + (d4 * weeks-into-current-stage)
  ]
  if weeks-since-infected >=  (stage1-duration + stage2-duration + stage3-duration + stage4-duration)  [ ;; we're in stage 5 (or past stage 4)
    report 7 ;; lethal level
  ]
end


to-report in-treatment?
  report last-treated = ticks
end




to-report risk-of-falling-out ;; AH: the falling out / reachieveing numbers are FLIPPED compared to Jenness' appendix. This gives us the right result thoguh.
  report (.0071 + .0102) / 2
end
to-report chance-of-re-achieving-suppression ;; AH:
  report (.00291 + 0.00066) / 2

end

to-report ever-treated?
  report time-on-art > 0
end

to-report serodiscordant-couples
  report links with [count both-ends with [hiv-positive?] = 1]
end
to-report hiv-pos-couples
  report links with [count both-ends with [hiv-positive?] = 2]
end
to-report hiv-neg-couples
  report links with [count both-ends with [hiv-positive?] = 0]
end

to-report weeks-since-infected
  report ticks - infected-at-tick
end

to-report is-acute?
  report weeks-since-infected < 12 and weeks-since-infected > 0
end

to-report log-odds-to-prob [logodds]
  report 1 / ( 1 + exp (-1 * logodds))
end


to test-untreated-viral-load [some-ticks]
  ca
  reset-ticks
  crt 1 [set hiv-positive? true set last-treated -1000]
  repeat some-ticks [
    tick
    ask turtles [
    set viral-load calculate-viral-load
;    show round viral-load
    ]
  ]
end

;; link reporter:
to-report UAIs-this-week
 report filter [[condom-use?] -> not condom-use? ] intercourses-this-week
end

to-report condom-usage-this-week ;; ONLY works for couples that actually had intercourses this week
  report length filter [[condom-use?] -> condom-use?] intercourses-this-week / length intercourses-this-week
end

to-report same-both-ends-as? [a-link]
  report [both-ends] of a-link = both-ends
end

to-report export-world-name
  report (word indicate-1a indicate-1b indicate-2a indicate-2b indicate-3a indicate-3b "__" random 1000)
end

to exp-w
  export-world (word "/exported_worlds/" export-world-name)
end

to serodiscordant-test [vl]
  ca
  crt 2
  ask turtle 0 [initialize-hiv+ vl]
  ask turtle 1 [initialize-hiv-]
  ask turtle 0 [create-link-with turtle 1]
  reset-ticks
  ask links [print (word "VL: " vl ": "risk-of-transmission-factors false)]
end

to initialize-hiv+ [vl]
  set hiv-positive? true
  set viral-load vl
  set infected-at-tick -1
  set sexual-role "insertive"
end

to initialize-hiv-
  set hiv-positive? false
  set on-prep? false
  set sexual-role "receptive"
  set ccr5-mutation 0
  set circumcized? false
end

to-report appropriate-breed
  if count my-main-links = 0 and count my-casual-links = 0 [ report M0C0S]
  if count my-main-links = 0 and count my-casual-links = 1 [ report M0C1S]
  if count my-main-links = 0 and count my-casual-links = 2 [ report M0C2S]
  if count my-main-links = 1 and count my-casual-links = 0 [ report M1C0S]
  if count my-main-links = 1 and count my-casual-links = 1 [ report M1C1S]
  if count my-main-links = 1 and count my-casual-links = 2 [ report M1C2S]
end

;to-report all-file-names
;  let filename1 "d:/Dropbox (SESP)/CePIM work/Jenness Model/Network Replication/starting_states/network-"
;  let filename2 "
;  report map [n -> (word filename1 n filename2)] (range 1 50)
;end

to-report the-file-name
;  report (word "d:/Dropbox (SESP)/CePIM work/Jenness Model/Network Replication/starting_states/network-" file-n "
  report (word "starting_states/network-" file-n ".graphmloutput.graphml")
end

to-report #-intercourses-this-week
  report sum [ifelse-value (length intercourses-this-week > 0) [length intercourses-this-week] [0]] of my-links
end

to-report ccr5-to-number [ccr5-input]
  if ccr5-input = "WW" [report 0]
  if ccr5-input = "DW" [report 1]
  if ccr5-input = "DD" [report 2]
  report 0
end

to-report to-bool [avar]
  if is-string? avar [set avar runresult avar]
  if avar = 0 [report false]
  if avar = 1 [report true]
  report "ERROR"
  ;; we want it to fail if it's neither, so allow it to run reporter to end without report
end
to-report prevalence
  report count turtles with [hiv-positive?] / count turtles
end

to go-one-tick-comparison
  random-seed a-random-number
  ask links [
    set intercourses-this-week (list)
    set duration duration - 1
    set transmission-this-week false
  ]

  ;; all relationships have AI some number of times
  ask links [
    set intercourses-this-week map [ [ ] -> using-condom?] range calculate-intercourses-this-week  ;; this gives us a list of bools for whether or not they used condom


    ;; now we calculate the risks of HIV transmission. We only do this for serodiscordant couples
    if count both-ends with [hiv-positive?] = 1  [
      ;; ah: replaced these with reporters
;      let hiv- one-of both-ends with [not hiv-positive?]
;      let hiv+ [other-end] of hiv-
;

      ;; we create a list with booleans for each intercourse, seeing if the hiv- was infected
      let infection-bool-list map [ [ condom? ] -> random-float 1 < risk-of-transmission-factors condom?] intercourses-this-week
      ;; filter it and see how long it is. I'm sure there's a reduce or sentence for this.
      if length filter [[infected?] -> infected?] infection-bool-list > 0 [ ;;
        set transmission-this-week true
        ask hiv- [
          set hiv-pos-count hiv-pos-count + 1
          set hiv-positive? true set infected-at-tick ticks]
      ]
    ]
  ]
  tick

end

to-report  a-random-number
  report runresult reduce word  (sentence substring date-and-time 0 2  substring date-and-time 3 5 substring date-and-time 6 8  substring date-and-time 9 12 )
end

to-report AI_scale
;  report 1.323
  report 1.529
end

to-report risk-of-transmission-factors [condom?]
  ;; AH: replaced these with reporters
;  let hiv- one-of both-ends with [not hiv-positive?]
;  let hiv+ one-of both-ends with [hiv-positive?]
  if hiv- = nobody or hiv+ = nobody [report 0]

  if [viral-load] of hiv+ = 0 [report 0]

  let hiv-neg-ai-position 0
  if [sexual-role = "receptive"] of hiv- or [sexual-role = "insertive"] of hiv+ [set hiv-neg-ai-position "receptive"]
  if [sexual-role = "insertive"] of hiv- or [sexual-role = "receptive"] of hiv+ [set hiv-neg-ai-position "insertive"]
  if [sexual-role = "versatile"] of hiv- and  [sexual-role = "versatile"] of hiv+
  [
    ifelse random-float 1 < .49 [
      set hiv-neg-ai-position "IEV"
    ]
    [
      ifelse ([insertivity-preference] of hiv- / ([insertivity-preference] of hiv- + [insertivity-preference] of hiv+)) > random-float 1 [
        set hiv-neg-ai-position "insertive"
      ]
      [
        set hiv-neg-ai-position  "receptive"
      ]
    ]
  ]

  let receptive-risk 0.008938
  let insertive-risk 0.003379
  ;; base risk based on role
  set receptive-risk receptive-risk * 2.45 ^ ([viral-load] of hiv+ - 4.5)
  set insertive-risk insertive-risk * 2.45 ^ ([viral-load] of hiv+ - 4.5)

  if hiv-neg-ai-position = "insertive" or hiv-neg-ai-position = "IEV" and [circumcized?] of hiv- [
    set insertive-risk insertive-risk * 0.4
  ]


;; CCR5 factor
  if [CCR5-mutation] of hiv- = 2 [
  set receptive-risk 0
  set insertive-risk 0
  ]
    ;this represents -inf but Netlogo cant work with that afaik, using 100 to make sure we do not run into the problem of too large a number when taking the exp of it later on
  if [CCR5-mutation] of hiv- = 1 [
  set receptive-risk receptive-risk * 0.3
  set insertive-risk insertive-risk * 0.3
  ]

  if condom? [
  set receptive-risk receptive-risk * 0.295
  set insertive-risk insertive-risk * 0.295
  ]

;;  on prep?
if [on-prep?] of hiv-
  [
;    set risk risk * (1 - [prep-risk-reduction] of hiv-)
  set receptive-risk receptive-risk * (1 - [prep-risk-reduction] of hiv-)
  set insertive-risk insertive-risk * (1 - [prep-risk-reduction] of hiv-)

  ]

;; acute stage?
if [is-acute?] of hiv+ [
;    set risk risk * 6
  set receptive-risk receptive-risk * 6
  set insertive-risk insertive-risk * 6
  ]
  let risk 0

;  show (list insertive-risk receptive-risk risk)

  if hiv-neg-ai-position = "IEV" [ set risk 1 - ((1 - insertive-risk) * (1 - receptive-risk))]
  if hiv-neg-ai-position = "insertive" [set risk insertive-risk]
  if hiv-neg-ai-position = "receptive" [ set risk receptive-risk]

;  show (list insertive-risk receptive-risk risk)


report risk
end



to go-n-tick-comparison [n]
  reset-ticks
  repeat n [

    ask links [
      set intercourses-this-week (list)
      set duration duration - 1
      set transmission-this-week false
    ]
    ;; all relationships have AI some number of times
    ask links [
      set intercourses-this-week map [ [ ] -> using-condom?] range calculate-intercourses-this-week  ;; this gives us a list of bools for whether or not they used condom
                                                                                                     ;    show
                                                                                                     ;; now we calculate the risks of HIV transmission. We only do this for serodiscordant couples
      if count both-ends with [hiv-positive?] = 1  [
        ;;AH: replaced these with reporters
;        let hiv- one-of both-ends with [not hiv-positive?]
;        let hiv+ [other-end] of hiv-
        ;; we create a list with booleans for each intercourse, seeing if the hiv- was infected
        let infection-bool-list map [ [ condom? ] -> random-float 1 < risk-of-transmission-factors condom?] intercourses-this-week
        ;; filter it and see how long it is. I'm sure there's a reduce or sentence for this.
        if length filter [[infected?] -> infected?] infection-bool-list > 0 [ ;;
          set transmission-this-week true
          ask hiv- [
            set hiv-pos-count hiv-pos-count + 1
            set hiv-positive? true set infected-at-tick ticks]
        ]
      ]
    ]
;    show count turtles with [not hiv-on-import? and hiv-positive?]
    let incidence count turtles with [not hiv-on-import? and hiv-positive?]
    tick
    let total-intercourses sum map [ l -> length l] [                                      intercourses-this-week] of links
    let UAIs               sum map [ l -> length l] [ filter [ intercourse -> intercourse] intercourses-this-week] of links
    file-print csv:to-row (list ticks prevalence incidence total-intercourses UAIs)

  ]
end

to write-to-log

end

to run-n-ticks-m-times [n m]
  open-graphml
  show (word "opening world " file-n)
  file-open (word "world_" file-n "_for_" m "ticks_" a-random-number ".csv")
  file-print csv:to-row ["tick" "prevalence" "incidence" "intercourses" "UAIs"]
  file-print csv:to-row (list 0 prevalence file-n )
  let counter 0
  repeat m [
    ask turtles [set hiv-positive? hiv-on-import?]
    go-n-tick-comparison n
    file-print csv:to-row (list counter "-" "-")
    if counter mod 500 = 0 [file-flush

    ]
    if counter mod 5000 = 0 [
      show (list date-and-time counter)]
    set counter counter + 1
  ]
  file-flush
  file-close
  tick
  tick
  tick
  tick
end

to open-graphml
  let n file-n
  ca
  set file-n n
  nw:load-graphml the-file-name

  ask turtles [
    set on-prep? false

    set circumcized? to-bool circumcized?
    set diagnosed? to-bool diagnosed?
    set hiv-positive? to-bool hiv-positive?
    set hiv-on-import? hiv-positive?
    set age-weeks round (age-weeks * 52)
    set african-american? ifelse-value (african-american? = "B") [true] [false]
    set condom-always-pref to-bool condom-always-pref
    set always-condom-casual? to-bool always-condom-casual?
    set always-condom-onetime? to-bool always-condom-onetime?



    set ccr5-mutation ccr5-to-number ccr5-mutation
    ifelse stage < 3 and stage > 0[ ;; this means that they are acute in the R code
      set infected-at-tick -2 ;; then we set their infection to a few ticks prior. This does not affect the behiavior AS LONG AS WE DO NOT RE-CALCULATE THEIR VIRAL LOAD! So don't do that. M'kay?
    ]
    [
      set infected-at-tick 5
    ]

    set sexual-role sexrole-data-input
  ]
  ask links [set hiv-disclosed? to-bool hiv-disclosed]
reset-ticks
end

to-report sexrole-data-input
  (ifelse
    sexual-role = "R" [report "receptive"]
    sexual-role = "V" [report "versatile"]
    sexual-role = "I" [report "insertive"]
    [report "ERROR"])
end

to print-link-probabilities
  file-open "Probabilities-per-link.csv"
  file-print csv:to-row ["v_name1" "v_name2" "probability"]
  ask serodiscordant-couples [
    file-print csv:to-row (list [name] of end1 [name] of end2 probability-of-using-condom)
  ]
  file-close
end

to-report link-with-ids [id1 id2]
  report one-of links with [any? both-ends with [name = id1] and any? both-ends with [name = id2]]
end

to-report transmission-risks
  report remove-duplicates map [ -> risk-of-transmission-factors one-of [true false]] range 1
end

to-report hiv-
  report one-of both-ends with [not hiv-positive?]
end
to-report hiv+
  report one-of both-ends with [hiv-positive?]
end

to-report prob-of-hiv--insertive ;; probability that the HIV-negative partner will be insertive
  if [sexual-role = "receptive"] of hiv- or [sexual-role = "insertive"] of hiv+ [report 0]
  if [sexual-role = "insertive"] of hiv- or [sexual-role = "receptive"] of hiv+ [report 1]
  report ([insertivity-preference] of hiv- / ([insertivity-preference] of hiv- + [insertivity-preference] of hiv+))
end

to write-sexual-position-probs [world-ids]
  file-open "Sexual_Position_probabilities.csv"
  file-print csv:to-row (list "World-id" "ID of HIV+" "ID of HIV-" "Prob of HIV- being Insertive")
  foreach world-ids [ id ->
    show id
    set file-n id
    open-graphml
    ask serodiscordant-couples [
      let data-list []
      set data-list lput id data-list
      set data-list lput [name] of hiv+ data-list
      set data-list lput [name] of hiv- data-list
      set data-list lput prob-of-hiv--insertive data-list
      file-print csv:to-row data-list
    ]
  ]
  file-close
end

to write-average-risk-test-n-times [n]
  ask serodiscordant-couples[
  file-open (word "/average_risk_test/" [name] of hiv+ "_" [name] of hiv- ".csv")
    repeat n [
      set intercourses-this-week map [ [ ] -> using-condom?] range calculate-intercourses-this-week
      let transmission-prob-list map [ [ condom? ] -> risk-of-transmission-factors condom?] intercourses-this-week
      file-print csv:to-row transmission-prob-list
;      show csv:to-row transmission-prob-list
    ]
    file-close
  ]

end

to update-prep-conditions
  set indicate-1a false
  set indicate-1b false
  set indicate-2a false
  set indicate-2b false
  set indicate-3a false
  set indicate-3b false


  if prep-condition = 1 [set indicate-1a true]
  if prep-condition = 2 [set indicate-1b true]
  if prep-condition = 3 [set indicate-2a true]
  if prep-condition = 4 [set indicate-2b true]
  if prep-condition = 5 [set indicate-3a true]
  if prep-condition = 6 [set indicate-3b true]
  if prep-condition = 7 [set indicate-1a true set indicate-2a true set indicate-3a true]
  if prep-condition = 8 [set indicate-1b true set indicate-2b true set indicate-3a true]
  if prep-condition = 9 [set indicate-1b true set indicate-2b true set indicate-3b true]
end


to-report incidence-last-week
  report count turtles with [infected-at-tick = ticks - 1 and hiv-positive?]
end

to-report #-of-UAIs
  if  [uais-this-week] of links != [] [ ;; this is for behaviorspace so it doesn't run reduce on an empty list
    report length reduce sentence [uais-this-week] of links
  ]
  report 0
end
to-report #-of-AIs
  if [intercourses-this-week] of links != [][ ;; for behavior space
    report   length reduce sentence [intercourses-this-week] of links
  ]
  report 0
end

to test-viral-progression-1
  ca
  reset-ticks
  crt 1 [
    set hiv-positive? true
    set infected-at-tick ticks
  ]
  repeat 15 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 600 [tick]
  repeat 50 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
      tick
  ]
end


to test-viral-progression-2
  ca
  reset-ticks
  crt 1 [
    set full-suppression? true
    set hiv-positive? true
    set infected-at-tick ticks
  ]

  repeat 15 [
    ask turtles [
set time-on-ART time-on-ART + 1
      set last-treated ticks
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 600 [tick]
  repeat 50 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
      tick
  ]
end

to test-viral-progression-3
  ca
  reset-ticks
  crt 1 [
    set full-suppression? true
    set hiv-positive? true
    set infected-at-tick ticks
  ]
  repeat 2 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  ask turtles [
    set time-on-ART time-on-ART + 1
     set last-treated ticks
    set viral-load calculate-viral-load
    show (list ticks viral-load)
  ]
  tick
  repeat 14 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 600 [tick]
  repeat 25 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
      tick
  ]
end

to test-viral-progression-4
  ca
  reset-ticks
  crt 1 [
    set full-suppression? true
    set hiv-positive? true
    set infected-at-tick ticks
  ]
  repeat 2 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 3 [
    ask turtles [
      set time-on-ART time-on-ART + 1
       set last-treated ticks
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 12 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 600 [tick]
  repeat 25 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
      tick
  ]
end


to test-viral-progression-5
  ca
  reset-ticks
  crt 1 [
    set full-suppression? true
    set hiv-positive? true
    set infected-at-tick ticks
  ]
  repeat 8 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 2 [
    ask turtles [
      set time-on-ART time-on-ART + 1
       set last-treated ticks
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 6 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 600 [tick]
  repeat 25 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
      tick
  ]
end


to test-viral-progression-6
  ca
  reset-ticks
  crt 1 [
    set full-suppression? true
    set hiv-positive? true
    set infected-at-tick ticks
  ]
  repeat 20 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 5 [
    ask turtles [
      set time-on-ART time-on-ART + 1
       set last-treated ticks
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]
  repeat 20 [
    ask turtles [
      set viral-load calculate-viral-load
      show (list ticks viral-load)
    ]
    tick
  ]

end
@#$#@#$#@
GRAPHICS-WINDOW
540
45
733
239
-1
-1
185.0
1
10
1
1
1
0
1
1
1
0
0
0
0
1
1
1
ticks
30.0

BUTTON
5
195
170
228
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
5
160
165
194
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
4
10
174
43
n-people
n-people
0
10000
1000.0
100
1
NIL
HORIZONTAL

TEXTBOX
1035
10
1185
28
Relationship type counts
11
0.0
1

SWITCH
5
45
165
78
infect-on-setup?
infect-on-setup?
0
1
-1000

SWITCH
5
80
165
113
distributed-infection?
distributed-infection?
0
1
-1000

SWITCH
15
245
105
278
PrEP?
PrEP?
0
1
-1000

TEXTBOX
20
230
170
248
PrEP variables
11
0.0
1

SLIDER
15
280
295
313
indication-window
indication-window
12
48
26.0
1
1
weeks/ticks
HORIZONTAL

SLIDER
15
405
260
438
partner-testing-window-ind1
partner-testing-window-ind1
0
26
26.0
1
1
NIL
HORIZONTAL

SWITCH
15
440
132
473
indicate-1a
indicate-1a
1
1
-1000

SWITCH
140
440
257
473
indicate-1b
indicate-1b
1
1
-1000

SWITCH
15
500
132
533
indicate-2a
indicate-2a
1
1
-1000

SWITCH
135
500
252
533
indicate-2b
indicate-2b
1
1
-1000

SWITCH
15
560
132
593
indicate-3a
indicate-3a
0
1
-1000

SWITCH
135
560
252
593
indicate-3b
indicate-3b
1
1
-1000

TEXTBOX
20
385
80
403
Prep 1
11
0.0
1

SLIDER
120
245
297
278
prep-coverage-fraction
prep-coverage-fraction
0
1
0.4
.01
1
NIL
HORIZONTAL

SWITCH
355
45
505
78
diagnosed-on-setup?
diagnosed-on-setup?
0
1
-1000

SWITCH
165
80
332
113
treatment-on-setup?
treatment-on-setup?
0
1
-1000

TEXTBOX
915
315
1065
356
Jenness' model has around 26% after it stabilizes. We have slightly higher.
11
0.0
1

SWITCH
165
45
352
78
age-shifted-prevalence?
age-shifted-prevalence?
0
1
-1000

SWITCH
5
120
212
153
sexual-activity-network?
sexual-activity-network?
1
1
-1000

SWITCH
180
10
312
43
test-versatile?
test-versatile?
1
1
-1000

SWITCH
20
630
137
663
indicate-4a
indicate-4a
1
1
-1000

TEXTBOX
445
445
595
556
print:\n- prevalnce at each tick\n- incidence for each tick\n- populatio nsize for each tick\n- UAI\n- AIs\n- deaths, etc.\n- people on prep
11
0.0
1

CHOOSER
325
260
463
305
prep-condition
prep-condition
0 1 2 3 4 5 6 7 8 9
5

PLOT
740
40
1125
275
prevalence
NIL
NIL
0.0
10.0
0.1
0.3
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot prevalence"

@#$#@#$#@
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.1
@#$#@#$#@
need-to-manually-make-preview-for-this-model
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="TickComparison" repetitions="100" runMetricsEveryStep="true">
    <setup>open-graphml</setup>
    <go>go-one-tick-comparison</go>
    <timeLimit steps="5"/>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count turtles</metric>
    <metric>file-n</metric>
    <steppedValueSet variable="file-n" first="1" step="1" last="20"/>
  </experiment>
  <experiment name="experiment" repetitions="1" runMetricsEveryStep="false">
    <setup>run-n-ticks-m-times 1 10000</setup>
    <timeLimit steps="2"/>
    <metric>1</metric>
    <steppedValueSet variable="file-n" first="1" step="1" last="250"/>
  </experiment>
  <experiment name="2k-ticks-0" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>burned-in-setup</setup>
    <go>go</go>
    <timeLimit steps="520"/>
    <metric>file-n</metric>
    <metric>prevalence</metric>
    <metric>incidence-last-week</metric>
    <metric>count turtles</metric>
    <metric>#-of-UAIs</metric>
    <metric>#-of-AIs</metric>
    <metric>count naturally-dying-turtles</metric>
    <metric>count aids-dying-turtles</metric>
    <metric>count turtles with [on-prep?]</metric>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count (turtle-set [hiv-] of serodiscordant-couples)</metric>
    <metric>initial-random-seed</metric>
    <steppedValueSet variable="file-n" first="0" step="1" last="49"/>
    <steppedValueSet variable="prep-condition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="2k-ticks-50" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>burned-in-setup</setup>
    <go>go</go>
    <timeLimit steps="520"/>
    <metric>file-n</metric>
    <metric>prevalence</metric>
    <metric>incidence-last-week</metric>
    <metric>count turtles</metric>
    <metric>#-of-UAIs</metric>
    <metric>#-of-AIs</metric>
    <metric>count naturally-dying-turtles</metric>
    <metric>count aids-dying-turtles</metric>
    <metric>count turtles with [on-prep?]</metric>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count (turtle-set [hiv-] of serodiscordant-couples)</metric>
    <metric>initial-random-seed</metric>
    <steppedValueSet variable="file-n" first="50" step="1" last="99"/>
    <steppedValueSet variable="prep-condition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="2k-ticks-100" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>burned-in-setup</setup>
    <go>go</go>
    <timeLimit steps="520"/>
    <metric>file-n</metric>
    <metric>prevalence</metric>
    <metric>incidence-last-week</metric>
    <metric>count turtles</metric>
    <metric>#-of-UAIs</metric>
    <metric>#-of-AIs</metric>
    <metric>count naturally-dying-turtles</metric>
    <metric>count aids-dying-turtles</metric>
    <metric>count turtles with [on-prep?]</metric>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count (turtle-set [hiv-] of serodiscordant-couples)</metric>
    <metric>initial-random-seed</metric>
    <steppedValueSet variable="file-n" first="100" step="1" last="149"/>
    <steppedValueSet variable="prep-condition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="2k-ticks-150" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>burned-in-setup</setup>
    <go>go</go>
    <timeLimit steps="520"/>
    <metric>file-n</metric>
    <metric>prevalence</metric>
    <metric>incidence-last-week</metric>
    <metric>count turtles</metric>
    <metric>#-of-UAIs</metric>
    <metric>#-of-AIs</metric>
    <metric>count naturally-dying-turtles</metric>
    <metric>count aids-dying-turtles</metric>
    <metric>count turtles with [on-prep?]</metric>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count (turtle-set [hiv-] of serodiscordant-couples)</metric>
    <metric>initial-random-seed</metric>
    <steppedValueSet variable="file-n" first="150" step="1" last="199"/>
    <steppedValueSet variable="prep-condition" first="0" step="1" last="9"/>
  </experiment>
  <experiment name="2k-ticks-200" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="true">
    <setup>burned-in-setup</setup>
    <go>go</go>
    <timeLimit steps="520"/>
    <metric>file-n</metric>
    <metric>prevalence</metric>
    <metric>incidence-last-week</metric>
    <metric>count turtles</metric>
    <metric>#-of-UAIs</metric>
    <metric>#-of-AIs</metric>
    <metric>count naturally-dying-turtles</metric>
    <metric>count aids-dying-turtles</metric>
    <metric>count turtles with [on-prep?]</metric>
    <metric>count turtles with [hiv-positive?]</metric>
    <metric>count (turtle-set [hiv-] of serodiscordant-couples)</metric>
    <metric>initial-random-seed</metric>
    <steppedValueSet variable="file-n" first="200" step="1" last="249"/>
    <steppedValueSet variable="prep-condition" first="0" step="1" last="9"/>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
