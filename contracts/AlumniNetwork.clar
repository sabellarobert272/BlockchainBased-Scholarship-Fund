;; Alumni Impact Network & Success Tracking System
;; Creates sustainable scholarship ecosystem with mentorship and impact verification

(define-constant ERR-NOT-AUTHORIZED (err u200))
(define-constant ERR-ALUMNI-NOT-FOUND (err u201))
(define-constant ERR-ALREADY-REGISTERED (err u202))
(define-constant ERR-INVALID-DATA (err u203))
(define-constant ERR-MENTORSHIP-FULL (err u204))
(define-constant ERR-NOT-ELIGIBLE (err u205))
(define-constant ERR-IMPACT-NOT-FOUND (err u206))
(define-constant ERR-NOT-FOUND (err u207))

;; Admin and contract reference
(define-data-var admin principal tx-sender)
(define-data-var scholarship-contract principal tx-sender)

;; ID trackers
(define-data-var next-alumni-id uint u1)
(define-data-var next-impact-id uint u1)
(define-data-var next-mentorship-id uint u1)

;; Core alumni registry with career tracking
(define-map AlumniRegistry
    uint
    {
        alumni-address: principal,
        graduation-year: uint,
        degree-field: (string-ascii 64),
        initial-scholarship-amount: uint,
        current-salary: uint,
        career-level: (string-ascii 32),
        company: (string-ascii 64),
        verified: bool,
        registered-at: uint,
        last-update: uint,
        mentorship-slots: uint,
        total-mentees: uint,
        giving-back-total: uint
    }
)

;; Alumni lookup by address
(define-map AlumniLookup
    principal
    uint
)

;; Impact tracking for ROI calculations
(define-map ImpactReports
    uint
    {
        alumni-id: uint,
        report-type: (string-ascii 32),
        salary-increase: uint,
        promotion-achieved: bool,
        skills-gained: (string-ascii 128),
        community-impact: (string-ascii 256),
        submitted-at: uint,
        verified: bool,
        impact-score: uint
    }
)

;; Mentorship program structure
(define-map MentorshipPrograms
    uint
    {
        mentor-alumni-id: uint,
        mentee-student: principal,
        field-of-study: (string-ascii 64),
        session-count: uint,
        start-date: uint,
        end-date: uint,
        status: (string-ascii 20),
        mentor-rating: uint,
        mentee-rating: uint,
        completion-bonus-earned: uint
    }
)

;; Student mentorship applications
(define-map MentorshipApplications
    principal
    {
        field-interest: (string-ascii 64),
        career-goals: (string-ascii 256),
        preferred-mentor-id: (optional uint),
        application-date: uint,
        status: (string-ascii 20)
    }
)

;; Alumni giving back tracking
(define-map AlumniContributions
    uint
    {
        alumni-id: uint,
        contribution-type: (string-ascii 32),
        amount: uint,
        target-field: (optional (string-ascii 64)),
        contributed-at: uint,
        impact-multiplier: uint
    }
)

;; Register new alumni with career information
(define-public (register-alumni (graduation-year uint) (degree-field (string-ascii 64)) (scholarship-amount uint) (current-salary uint) (career-level (string-ascii 32)) (company (string-ascii 64)))
    (let
        ((alumni-id (var-get next-alumni-id))
         (current-block stacks-block-height))
        (begin
            ;; Validate input data
            (asserts! (> graduation-year u2000) ERR-INVALID-DATA)
            (asserts! (> current-salary u0) ERR-INVALID-DATA)
            (asserts! (> scholarship-amount u0) ERR-INVALID-DATA)
            
            ;; Check if already registered
            (asserts! (is-none (map-get? AlumniLookup tx-sender)) ERR-ALREADY-REGISTERED)
            
            ;; Register alumni
            (map-set AlumniRegistry alumni-id
                {
                    alumni-address: tx-sender,
                    graduation-year: graduation-year,
                    degree-field: degree-field,
                    initial-scholarship-amount: scholarship-amount,
                    current-salary: current-salary,
                    career-level: career-level,
                    company: company,
                    verified: false,
                    registered-at: current-block,
                    last-update: current-block,
                    mentorship-slots: u3,
                    total-mentees: u0,
                    giving-back-total: u0
                })
            
            ;; Create lookup entry
            (map-set AlumniLookup tx-sender alumni-id)
            
            ;; Increment ID counter
            (var-set next-alumni-id (+ alumni-id u1))
            
            (ok alumni-id))))

;; Admin verifies alumni registration
(define-public (verify-alumni (alumni-id uint))
    (let
        ((alumni (unwrap! (map-get? AlumniRegistry alumni-id) ERR-ALUMNI-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (not (get verified alumni)) ERR-NOT-ELIGIBLE)
            
            (map-set AlumniRegistry alumni-id
                (merge alumni { verified: true }))
            
            (ok true))))

;; Alumni submit impact reports for transparency
(define-public (submit-impact-report (report-type (string-ascii 32)) (salary-increase uint) (promotion-achieved bool) (skills-gained (string-ascii 128)) (community-impact (string-ascii 256)))
    (let
        ((alumni-id (unwrap! (map-get? AlumniLookup tx-sender) ERR-ALUMNI-NOT-FOUND))
         (alumni (unwrap! (map-get? AlumniRegistry alumni-id) ERR-ALUMNI-NOT-FOUND))
         (impact-id (var-get next-impact-id))
         (current-block stacks-block-height)
         (calculated-score (+ salary-increase (if promotion-achieved u50 u0))))
        (begin
            (asserts! (get verified alumni) ERR-NOT-ELIGIBLE)
            
            (map-set ImpactReports impact-id
                {
                    alumni-id: alumni-id,
                    report-type: report-type,
                    salary-increase: salary-increase,
                    promotion-achieved: promotion-achieved,
                    skills-gained: skills-gained,
                    community-impact: community-impact,
                    submitted-at: current-block,
                    verified: false,
                    impact-score: calculated-score
                })
            
            (var-set next-impact-id (+ impact-id u1))
            (ok impact-id))))

;; Students apply for mentorship
(define-public (apply-for-mentorship (field-interest (string-ascii 64)) (career-goals (string-ascii 256)) (preferred-mentor-id (optional uint)))
    (let
        ((current-block stacks-block-height))
        (begin
            ;; Check if student has existing application
            (asserts! (is-none (map-get? MentorshipApplications tx-sender)) ERR-ALREADY-REGISTERED)
            
            (map-set MentorshipApplications tx-sender
                {
                    field-interest: field-interest,
                    career-goals: career-goals,
                    preferred-mentor-id: preferred-mentor-id,
                    application-date: current-block,
                    status: "pending"
                })
            
            (ok true))))

;; Admin matches mentor with student
(define-public (create-mentorship (mentor-alumni-id uint) (mentee-student principal) (field-of-study (string-ascii 64)))
    (let
        ((mentor (unwrap! (map-get? AlumniRegistry mentor-alumni-id) ERR-ALUMNI-NOT-FOUND))
         (mentorship-app (unwrap! (map-get? MentorshipApplications mentee-student) ERR-NOT-FOUND))
         (mentorship-id (var-get next-mentorship-id))
         (current-block stacks-block-height))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (get verified mentor) ERR-NOT-ELIGIBLE)
            (asserts! (> (get mentorship-slots mentor) (get total-mentees mentor)) ERR-MENTORSHIP-FULL)
            (asserts! (is-eq (get status mentorship-app) "pending") ERR-NOT-ELIGIBLE)
            
            ;; Create mentorship record
            (map-set MentorshipPrograms mentorship-id
                {
                    mentor-alumni-id: mentor-alumni-id,
                    mentee-student: mentee-student,
                    field-of-study: field-of-study,
                    session-count: u0,
                    start-date: current-block,
                    end-date: (+ current-block u8760), ;; One year duration
                    status: "active",
                    mentor-rating: u0,
                    mentee-rating: u0,
                    completion-bonus-earned: u0
                })
            
            ;; Update mentor's mentee count
            (map-set AlumniRegistry mentor-alumni-id
                (merge mentor { total-mentees: (+ (get total-mentees mentor) u1) }))
            
            ;; Update mentorship application status
            (map-set MentorshipApplications mentee-student
                (merge mentorship-app { status: "matched" }))
            
            (var-set next-mentorship-id (+ mentorship-id u1))
            (ok mentorship-id))))

;; Alumni contribute back to scholarship fund
(define-public (contribute-back (amount uint) (contribution-type (string-ascii 32)) (target-field (optional (string-ascii 64))))
    (let
        ((alumni-id (unwrap! (map-get? AlumniLookup tx-sender) ERR-ALUMNI-NOT-FOUND))
         (alumni (unwrap! (map-get? AlumniRegistry alumni-id) ERR-ALUMNI-NOT-FOUND))
         (current-block stacks-block-height)
         (multiplier (if (> (get total-mentees alumni) u2) u150 u100))) ;; 50% bonus for active mentors
        (begin
            (asserts! (get verified alumni) ERR-NOT-ELIGIBLE)
            (asserts! (> amount u0) ERR-INVALID-DATA)
            (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INVALID-DATA)
            
            ;; Transfer funds to scholarship contract
            (try! (stx-transfer? amount tx-sender (var-get scholarship-contract)))
            
            ;; Record contribution
            (map-set AlumniContributions alumni-id
                {
                    alumni-id: alumni-id,
                    contribution-type: contribution-type,
                    amount: amount,
                    target-field: target-field,
                    contributed-at: current-block,
                    impact-multiplier: multiplier
                })
            
            ;; Update alumni giving total
            (map-set AlumniRegistry alumni-id
                (merge alumni { giving-back-total: (+ (get giving-back-total alumni) amount) }))
            
            (ok true))))

;; Update mentorship session progress
(define-public (log-mentorship-session (mentorship-id uint))
    (let
        ((mentorship (unwrap! (map-get? MentorshipPrograms mentorship-id) ERR-NOT-FOUND))
         (alumni-id (get mentor-alumni-id mentorship)))
        (begin
            ;; Only mentor or mentee can log sessions
            (asserts! (or (is-eq tx-sender (get mentee-student mentorship))
                         (is-eq tx-sender (get alumni-address (unwrap! (map-get? AlumniRegistry alumni-id) ERR-ALUMNI-NOT-FOUND))))
                     ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status mentorship) "active") ERR-NOT-ELIGIBLE)
            
            (map-set MentorshipPrograms mentorship-id
                (merge mentorship { session-count: (+ (get session-count mentorship) u1) }))
            
            (ok true))))

;; Get alumni profile and impact summary
(define-read-only (get-alumni-profile (alumni-id uint))
    (let
        ((alumni (unwrap! (map-get? AlumniRegistry alumni-id) ERR-ALUMNI-NOT-FOUND)))
        (ok {
            alumni-info: alumni,
            roi-calculation: (if (> (get initial-scholarship-amount alumni) u0)
                             (/ (* (get current-salary alumni) u100) (get initial-scholarship-amount alumni))
                             u0),
            mentorship-capacity: (- (get mentorship-slots alumni) (get total-mentees alumni)),
            giving-ratio: (if (> (get current-salary alumni) u0)
                          (/ (* (get giving-back-total alumni) u100) (get current-salary alumni))
                          u0)
        })))

;; Get comprehensive network statistics
(define-read-only (get-network-statistics)
    (ok {
        total-alumni: (- (var-get next-alumni-id) u1),
        total-impact-reports: (- (var-get next-impact-id) u1),
        active-mentorships: (- (var-get next-mentorship-id) u1),
        network-health-score: (* (- (var-get next-alumni-id) u1) u10) ;; Simple scoring system
    }))

;; Get mentorship program details
(define-read-only (get-mentorship-details (mentorship-id uint))
    (ok (unwrap! (map-get? MentorshipPrograms mentorship-id) ERR-NOT-FOUND)))

;; Get alumni impact report
(define-read-only (get-impact-report (impact-id uint))
    (ok (unwrap! (map-get? ImpactReports impact-id) ERR-IMPACT-NOT-FOUND)))

;; Admin functions
(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)))

(define-public (set-scholarship-contract (contract-address principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set scholarship-contract contract-address)
        (ok true)))
