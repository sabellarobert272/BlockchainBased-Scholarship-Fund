(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-APPLIED (err u101))
(define-constant ERR-NOT-FOUND (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-INSUFFICIENT-FUNDS (err u104))
(define-constant ERR-NOT-ELIGIBLE (err u105))

(define-data-var fund-balance uint u0)
(define-data-var next-application-id uint u1)
(define-data-var admin principal tx-sender)


(define-constant ERR-PERFORMANCE-NOT-FOUND (err u112))
(define-constant ERR-PERFORMANCE-ALREADY-SUBMITTED (err u113))
(define-constant ERR-INVALID-PERFORMANCE-DATA (err u114))
(define-constant ERR-PERFORMANCE-INSUFFICIENT (err u115))
(define-constant ERR-INSTALLMENT-NOT-DUE (err u116))

(define-constant MIN-GPA-REQUIREMENT u300)
(define-constant MIN-CREDIT-HOURS u12)
(define-constant MIN-SERVICE-HOURS u20)
(define-constant PERFORMANCE-REVIEW-PERIOD u8760)

(define-data-var next-performance-id uint u1)

(define-map PerformanceTracking
    uint
    {
        scholarship-id: uint,
        student: principal,
        semester: uint,
        gpa: uint,
        credit-hours-completed: uint,
        community-service-hours: uint,
        graduation-progress: uint,
        verified: bool,
        submitted-at: uint,
        verified-by: (optional principal),
        meets-requirements: bool
    }
)

(define-map ScholarshipInstallments
    uint
    {
        student: principal,
        total-amount: uint,
        installments-total: uint,
        installments-paid: uint,
        installment-amount: uint,
        next-installment-due: uint,
        last-performance-check: uint,
        performance-streak: uint,
        active: bool,
        created-at: uint
    }
)

(define-map StudentPerformanceHistory
    principal
    {
        total-submissions: uint,
        approved-submissions: uint,
        current-streak: uint,
        last-submission: uint,
        scholarship-id: (optional uint)
    }
)

(define-map Applications
    uint 
    {
        applicant: principal,
        gpa: uint,
        major: (string-ascii 64),
        amount-requested: uint,
        status: (string-ascii 20)
    }
)

(define-map ApplicantStatus
    principal 
    {
        has-active-application: bool,
        received-funds: uint
    }
)

(define-public (initialize-contract)
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (ok true)))

(define-public (donate-to-fund)
    (let
        ((amount (stx-get-balance tx-sender)))
        (begin
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (var-set fund-balance (+ (var-get fund-balance) amount))
            (ok amount))))

(define-public (apply-for-scholarship (gpa uint) (major (string-ascii 64)) (amount uint))
    (let
        ((applicant-status (default-to 
            { has-active-application: false, received-funds: u0 }
            (map-get? ApplicantStatus tx-sender)))
         (application-id (var-get next-application-id)))
        (begin
            (asserts! (not (get has-active-application applicant-status)) ERR-ALREADY-APPLIED)
            (asserts! (>= gpa u300) ERR-NOT-ELIGIBLE)
            (asserts! (<= amount (var-get fund-balance)) ERR-INSUFFICIENT-FUNDS)
            (map-set Applications application-id
                {
                    applicant: tx-sender,
                    gpa: gpa,
                    major: major,
                    amount-requested: amount,
                    status: "pending"
                })
            (map-set ApplicantStatus tx-sender
                {
                    has-active-application: true,
                    received-funds: u0
                })
            (var-set next-application-id (+ application-id u1))
            (ok application-id))))

(define-public (approve-application (application-id uint))
    (let
        ((application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (try! (as-contract (stx-transfer? (get amount-requested application) tx-sender (get applicant application))))
            (var-set fund-balance (- (var-get fund-balance) (get amount-requested application)))
            (map-set Applications application-id
                (merge application { status: "approved" }))
            (map-set ApplicantStatus (get applicant application)
                {
                    has-active-application: false,
                    received-funds: (get amount-requested application)
                })
            (ok true))))

(define-public (reject-application (application-id uint))
    (let
        ((application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (map-set Applications application-id
                (merge application { status: "rejected" }))
            (map-set ApplicantStatus (get applicant application)
                {
                    has-active-application: false,
                    received-funds: u0
                })
            (ok true))))

(define-read-only (get-application (application-id uint))
    (ok (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND)))

(define-read-only (get-fund-balance)
    (ok (var-get fund-balance)))

(define-read-only (get-applicant-status (applicant principal))
    (ok (default-to 
        { has-active-application: false, received-funds: u0 }
        (map-get? ApplicantStatus applicant))))



(define-data-var total-applications uint u0)
(define-data-var total-approved uint u0) 
(define-data-var total-rejected uint u0)
(define-data-var total-funds-distributed uint u0)

(define-read-only (get-fund-statistics)
    (ok {
        total-applications: (var-get total-applications),
        total-approved: (var-get total-approved),
        total-rejected: (var-get total-rejected),
        total-funds-distributed: (var-get total-funds-distributed),
        current-balance: (var-get fund-balance)
    }))

;; Update the apply-for-scholarship function to add:
(var-set total-applications (+ (var-get total-applications) u1))

;; Update the approve-application function to add:
(var-set total-approved (+ (var-get total-approved) u1))
(var-set total-funds-distributed (+ (var-get total-funds-distributed) ))

;; Update the reject-application function to add:
(var-set total-rejected (+ (var-get total-rejected) u1))

(define-public (withdraw-application (application-id uint))
    (let (
        (application (unwrap! (map-get? Applications application-id) ERR-NOT-FOUND))
        )
        (begin
            (asserts! (is-eq tx-sender (get applicant application)) ERR-NOT-AUTHORIZED)
            (asserts! (is-eq (get status application) "pending") ERR-NOT-ELIGIBLE)
            (map-set Applications application-id
                (merge application { status: "withdrawn" }))
            (map-set ApplicantStatus tx-sender
                {
                    has-active-application: false,
                    received-funds: u0
                })
            (ok true))))


(define-constant MILESTONE-1 u1000000) 
(define-constant MILESTONE-2 u5000000)
(define-constant MILESTONE-3 u10000000)
(define-constant BONUS-AMOUNT u1000)

(define-map MilestoneReached
    uint
    bool
)

(define-public (claim-milestone-bonus)
    (let (
        (current-balance (var-get fund-balance))
        (milestone-1-claimed (default-to false (map-get? MilestoneReached u1)))
        (milestone-2-claimed (default-to false (map-get? MilestoneReached u2)))
        (milestone-3-claimed (default-to false (map-get? MilestoneReached u3)))
        )
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (if (and (>= current-balance MILESTONE-1) (not milestone-1-claimed))
                (begin
                    (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                    (map-set MilestoneReached u1 true)
                    (ok true)
                )
                (if (and (>= current-balance MILESTONE-2) (not milestone-2-claimed))
                    (begin
                        (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                        (map-set MilestoneReached u2 true)
                        (ok true)
                    )
                    (if (and (>= current-balance MILESTONE-3) (not milestone-3-claimed))
                        (begin
                            (try! (as-contract (stx-transfer? BONUS-AMOUNT tx-sender (var-get admin))))
                            (map-set MilestoneReached u3 true)
                            (ok true)
                        )
                        ERR-NOT-ELIGIBLE
                    )
                )
            )
        )
    ))

(define-read-only (get-milestone-status)
    (ok {
        milestone-1: (default-to false (map-get? MilestoneReached u1)),
        milestone-2: (default-to false (map-get? MilestoneReached u2)),
        milestone-3: (default-to false (map-get? MilestoneReached u3))
    }))

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)))

(define-constant ERR-RECURRING-NOT-FOUND (err u109))
(define-constant ERR-RECURRING-INACTIVE (err u110))
(define-constant ERR-PAYMENT-NOT-DUE (err u111))

(define-constant FREQUENCY-WEEKLY u1008)
(define-constant FREQUENCY-MONTHLY u4320)
(define-constant FREQUENCY-QUARTERLY u12960)

(define-data-var next-recurring-id uint u1)

(define-map RecurringDonations
    uint
    {
        donor: principal,
        amount: uint,
        frequency: uint,
        last-payment: uint,
        next-payment: uint,
        total-paid: uint,
        payment-count: uint,
        active: bool,
        created-at: uint
    }
)

(define-map DonorRecurring
    principal
    uint
)

(define-public (setup-recurring-donation (amount uint) (frequency uint))
    (let
        ((recurring-id (var-get next-recurring-id))
         (current-block stacks-block-height))
        (begin
            (asserts! (> amount u0) ERR-INVALID-AMOUNT)
            (asserts! (or (is-eq frequency FREQUENCY-WEEKLY) 
                         (or (is-eq frequency FREQUENCY-MONTHLY) 
                             (is-eq frequency FREQUENCY-QUARTERLY))) ERR-INVALID-AMOUNT)
            (asserts! (>= (stx-get-balance tx-sender) amount) ERR-INSUFFICIENT-FUNDS)
            (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
            (var-set fund-balance (+ (var-get fund-balance) amount))
            (map-set RecurringDonations recurring-id
                {
                    donor: tx-sender,
                    amount: amount,
                    frequency: frequency,
                    last-payment: current-block,
                    next-payment: (+ current-block frequency),
                    total-paid: amount,
                    payment-count: u1,
                    active: true,
                    created-at: current-block
                })
            (map-set DonorRecurring tx-sender recurring-id)
            (var-set next-recurring-id (+ recurring-id u1))
            (ok recurring-id))))

(define-public (process-recurring-payment (recurring-id uint))
    (let
        ((donation (unwrap! (map-get? RecurringDonations recurring-id) ERR-RECURRING-NOT-FOUND))
         (current-block stacks-block-height))
        (begin
            (asserts! (get active donation) ERR-RECURRING-INACTIVE)
            (asserts! (>= current-block (get next-payment donation)) ERR-PAYMENT-NOT-DUE)
            (asserts! (>= (stx-get-balance (get donor donation)) (get amount donation)) ERR-INSUFFICIENT-FUNDS)
            (try! (stx-transfer? (get amount donation) (get donor donation) (as-contract tx-sender)))
            (var-set fund-balance (+ (var-get fund-balance) (get amount donation)))
            (map-set RecurringDonations recurring-id
                (merge donation {
                    last-payment: current-block,
                    next-payment: (+ current-block (get frequency donation)),
                    total-paid: (+ (get total-paid donation) (get amount donation)),
                    payment-count: (+ (get payment-count donation) u1)
                }))
            (ok true))))

(define-public (cancel-recurring-donation (recurring-id uint))
    (let
        ((donation (unwrap! (map-get? RecurringDonations recurring-id) ERR-RECURRING-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (get donor donation)) ERR-NOT-AUTHORIZED)
            (asserts! (get active donation) ERR-RECURRING-INACTIVE)
            (map-set RecurringDonations recurring-id
                (merge donation { active: false }))
            (ok true))))

(define-public (update-recurring-amount (recurring-id uint) (new-amount uint))
    (let
        ((donation (unwrap! (map-get? RecurringDonations recurring-id) ERR-RECURRING-NOT-FOUND)))
        (begin
            (asserts! (is-eq tx-sender (get donor donation)) ERR-NOT-AUTHORIZED)
            (asserts! (get active donation) ERR-RECURRING-INACTIVE)
            (asserts! (> new-amount u0) ERR-INVALID-AMOUNT)
            (map-set RecurringDonations recurring-id
                (merge donation { amount: new-amount }))
            (ok true))))

(define-read-only (get-recurring-donation (recurring-id uint))
    (ok (unwrap! (map-get? RecurringDonations recurring-id) ERR-RECURRING-NOT-FOUND)))

(define-read-only (get-donor-recurring (donor principal))
    (ok (map-get? DonorRecurring donor)))

(define-read-only (get-due-payments (recurring-id uint))
    (let
        ((donation (unwrap! (map-get? RecurringDonations recurring-id) ERR-RECURRING-NOT-FOUND)))
        (ok {
            is-due: (and (get active donation) (>= stacks-block-height (get next-payment donation))),
            next-payment-block: (get next-payment donation),
            current-block: stacks-block-height
        })))

(define-read-only (get-recurring-stats)
    (ok {
        total-recurring-setups: (- (var-get next-recurring-id) u1),
        current-block: stacks-block-height
    }))



(define-public (setup-performance-based-scholarship (student principal) (total-amount uint) (installments uint))
    (let
        ((scholarship-id (var-get next-application-id))
         (installment-amount (/ total-amount installments))
         (current-block stacks-block-height))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (> total-amount u0) ERR-INVALID-AMOUNT)
            (asserts! (and (>= installments u2) (<= installments u8)) ERR-INVALID-AMOUNT)
            (asserts! (<= total-amount (var-get fund-balance)) ERR-INSUFFICIENT-FUNDS)
            (map-set ScholarshipInstallments scholarship-id
                {
                    student: student,
                    total-amount: total-amount,
                    installments-total: installments,
                    installments-paid: u0,
                    installment-amount: installment-amount,
                    next-installment-due: (+ current-block PERFORMANCE-REVIEW-PERIOD),
                    last-performance-check: current-block,
                    performance-streak: u0,
                    active: true,
                    created-at: current-block
                })
            (map-set StudentPerformanceHistory student
                {
                    total-submissions: u0,
                    approved-submissions: u0,
                    current-streak: u0,
                    last-submission: u0,
                    scholarship-id: (some scholarship-id)
                })
            (var-set next-application-id (+ scholarship-id u1))
            (ok scholarship-id))))

(define-public (submit-performance-data (scholarship-id uint) (semester uint) (gpa uint) (credit-hours uint) (service-hours uint) (graduation-progress uint))
    (let
        ((scholarship (unwrap! (map-get? ScholarshipInstallments scholarship-id) ERR-NOT-FOUND))
         (student-history (default-to 
             { total-submissions: u0, approved-submissions: u0, current-streak: u0, last-submission: u0, scholarship-id: none }
             (map-get? StudentPerformanceHistory tx-sender)))
         (performance-id (var-get next-performance-id))
         (current-block stacks-block-height))
        (begin
            (asserts! (is-eq tx-sender (get student scholarship)) ERR-NOT-AUTHORIZED)
            (asserts! (get active scholarship) ERR-NOT-ELIGIBLE)
            (asserts! (>= gpa u0) ERR-INVALID-PERFORMANCE-DATA)
            (asserts! (>= credit-hours u0) ERR-INVALID-PERFORMANCE-DATA)
            (asserts! (>= service-hours u0) ERR-INVALID-PERFORMANCE-DATA)
            (asserts! (and (>= graduation-progress u0) (<= graduation-progress u100)) ERR-INVALID-PERFORMANCE-DATA)
            (asserts! (>= current-block (get next-installment-due scholarship)) ERR-INSTALLMENT-NOT-DUE)
            (map-set PerformanceTracking performance-id
                {
                    scholarship-id: scholarship-id,
                    student: tx-sender,
                    semester: semester,
                    gpa: gpa,
                    credit-hours-completed: credit-hours,
                    community-service-hours: service-hours,
                    graduation-progress: graduation-progress,
                    verified: false,
                    submitted-at: current-block,
                    verified-by: none,
                    meets-requirements: false
                })
            (map-set StudentPerformanceHistory tx-sender
                (merge student-history {
                    total-submissions: (+ (get total-submissions student-history) u1),
                    last-submission: current-block
                }))
            (var-set next-performance-id (+ performance-id u1))
            (ok performance-id))))

(define-public (verify-performance-data (performance-id uint))
    (let
        ((performance (unwrap! (map-get? PerformanceTracking performance-id) ERR-PERFORMANCE-NOT-FOUND))
         (scholarship (unwrap! (map-get? ScholarshipInstallments (get scholarship-id performance)) ERR-NOT-FOUND))
         (student-history (unwrap! (map-get? StudentPerformanceHistory (get student performance)) ERR-NOT-FOUND))
         (meets-requirements (and 
             (>= (get gpa performance) MIN-GPA-REQUIREMENT)
             (>= (get credit-hours-completed performance) MIN-CREDIT-HOURS)
             (>= (get community-service-hours performance) MIN-SERVICE-HOURS))))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (not (get verified performance)) ERR-PERFORMANCE-ALREADY-SUBMITTED)
            (map-set PerformanceTracking performance-id
                (merge performance {
                    verified: true,
                    verified-by: (some tx-sender),
                    meets-requirements: meets-requirements
                }))
            (if meets-requirements
                (begin
                    (map-set StudentPerformanceHistory (get student performance)
                        (merge student-history {
                            approved-submissions: (+ (get approved-submissions student-history) u1),
                            current-streak: (+ (get current-streak student-history) u1)
                        }))
                    (ok true))
                (begin
                    (map-set StudentPerformanceHistory (get student performance)
                        (merge student-history { current-streak: u0 }))
                    (ok false))))))

(define-public (process-performance-payout (performance-id uint))
    (let
        ((performance (unwrap! (map-get? PerformanceTracking performance-id) ERR-PERFORMANCE-NOT-FOUND))
         (scholarship (unwrap! (map-get? ScholarshipInstallments (get scholarship-id performance)) ERR-NOT-FOUND))
         (current-block stacks-block-height))
        (begin
            (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
            (asserts! (get verified performance) ERR-NOT-ELIGIBLE)
            (asserts! (get meets-requirements performance) ERR-PERFORMANCE-INSUFFICIENT)
            (asserts! (get active scholarship) ERR-NOT-ELIGIBLE)
            (asserts! (< (get installments-paid scholarship) (get installments-total scholarship)) ERR-NOT-ELIGIBLE)
            (try! (as-contract (stx-transfer? (get installment-amount scholarship) tx-sender (get student scholarship))))
            (var-set fund-balance (- (var-get fund-balance) (get installment-amount scholarship)))
            (map-set ScholarshipInstallments (get scholarship-id performance)
                (merge scholarship {
                    installments-paid: (+ (get installments-paid scholarship) u1),
                    next-installment-due: (+ current-block PERFORMANCE-REVIEW-PERIOD),
                    last-performance-check: current-block,
                    performance-streak: (+ (get performance-streak scholarship) u1),
                    active: (< (+ (get installments-paid scholarship) u1) (get installments-total scholarship))
                }))
            (ok true))))

(define-read-only (get-scholarship-progress (scholarship-id uint))
    (let
        ((scholarship (unwrap! (map-get? ScholarshipInstallments scholarship-id) ERR-NOT-FOUND)))
        (ok {
            student: (get student scholarship),
            progress-percentage: (/ (* (get installments-paid scholarship) u100) (get installments-total scholarship)),
            amount-paid: (* (get installments-paid scholarship) (get installment-amount scholarship)),
            amount-remaining: (* (- (get installments-total scholarship) (get installments-paid scholarship)) (get installment-amount scholarship)),
            next-due: (get next-installment-due scholarship),
            performance-streak: (get performance-streak scholarship),
            active: (get active scholarship)
        })))

(define-read-only (get-student-performance-summary (student principal))
    (let
        ((history (default-to 
            { total-submissions: u0, approved-submissions: u0, current-streak: u0, last-submission: u0, scholarship-id: none }
            (map-get? StudentPerformanceHistory student))))
        (ok {
            total-submissions: (get total-submissions history),
            approved-submissions: (get approved-submissions history),
            approval-rate: (if (> (get total-submissions history) u0) 
                (/ (* (get approved-submissions history) u100) (get total-submissions history)) 
                u0),
            current-streak: (get current-streak history),
            last-submission: (get last-submission history),
            has-active-scholarship: (is-some (get scholarship-id history))
        })))

(define-read-only (get-performance-data (performance-id uint))
    (ok (unwrap! (map-get? PerformanceTracking performance-id) ERR-PERFORMANCE-NOT-FOUND)))

(define-read-only (get-performance-requirements)
    (ok {
        minimum-gpa: MIN-GPA-REQUIREMENT,
        minimum-credit-hours: MIN-CREDIT-HOURS,
        minimum-service-hours: MIN-SERVICE-HOURS,
        review-period-blocks: PERFORMANCE-REVIEW-PERIOD
    }))








