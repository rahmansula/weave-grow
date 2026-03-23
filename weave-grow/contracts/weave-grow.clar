;; WeaveGrow - Decentralized Social Impact Platform
;; 
;; Core features:
;; - Charitable project registration and management
;; - Donation escrow with milestone-based release
;; - Impact scoring and validation
;; - Community staking for Impact Weavers
;; - Ripple Multiplier for successful projects

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-invalid-amount (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-already-exists (err u105))
(define-constant err-invalid-milestone (err u106))
(define-constant err-milestone-not-ready (err u107))
(define-constant err-insufficient-stake (err u108))

;; Minimum stake required to become an Impact Weaver (in microSTX)
(define-constant min-stake-amount u1000000)

;; Impact multiplier percentage (basis points - 100 = 1%)
(define-constant ripple-multiplier u500)

;; Data Variables
(define-data-var project-nonce uint u0)
(define-data-var milestone-nonce uint u0)

;; Data Maps

;; Projects storage
(define-map projects
    { project-id: uint }
    {
        owner: principal,
        name: (string-ascii 50),
        description: (string-ascii 500),
        total-raised: uint,
        total-released: uint,
        impact-score: uint,
        active: bool,
        sdg-category: uint,
        created-at: uint
    }
)

;; Milestones for each project
(define-map milestones
    { milestone-id: uint }
    {
        project-id: uint,
        description: (string-ascii 200),
        target-amount: uint,
        released: bool,
        verified: bool,
        verification-count: uint,
        created-at: uint
    }
)

;; Donations tracking
(define-map donations
    { donor: principal, project-id: uint }
    {
        amount: uint,
        timestamp: uint
    }
)

;; Project escrow balances
(define-map project-balances
    { project-id: uint }
    { balance: uint }
)

;; Impact Weavers (validators) staking
(define-map impact-weavers
    { weaver: principal }
    {
        stake-amount: uint,
        validations: uint,
        reputation: uint,
        active: bool
    }
)

;; Milestone verifications by weavers
(define-map milestone-verifications
    { milestone-id: uint, weaver: principal }
    { verified: bool }
)

;; Impact cluster relationships
(define-map impact-clusters
    { parent-project: uint, child-project: uint }
    { active: bool }
)

;; Read-only functions

(define-read-only (get-project (project-id uint))
    (map-get? projects { project-id: project-id })
)

(define-read-only (get-milestone (milestone-id uint))
    (map-get? milestones { milestone-id: milestone-id })
)

(define-read-only (get-project-balance (project-id uint))
    (default-to { balance: u0 } (map-get? project-balances { project-id: project-id }))
)

(define-read-only (get-donation (donor principal) (project-id uint))
    (map-get? donations { donor: donor, project-id: project-id })
)

(define-read-only (get-impact-weaver (weaver principal))
    (map-get? impact-weavers { weaver: weaver })
)

(define-read-only (is-impact-weaver (weaver principal))
    (match (map-get? impact-weavers { weaver: weaver })
        weaver-data (get active weaver-data)
        false
    )
)

(define-read-only (get-cluster-relationship (parent uint) (child uint))
    (map-get? impact-clusters { parent-project: parent, child-project: child })
)

;; Public functions

;; Register a new charitable project
(define-public (create-project (name (string-ascii 50)) (description (string-ascii 500)) (sdg-category uint))
    (let
        (
            (new-project-id (+ (var-get project-nonce) u1))
        )
        (map-set projects
            { project-id: new-project-id }
            {
                owner: tx-sender,
                name: name,
                description: description,
                total-raised: u0,
                total-released: u0,
                impact-score: u0,
                active: true,
                sdg-category: sdg-category,
                created-at: block-height
            }
        )
        (map-set project-balances
            { project-id: new-project-id }
            { balance: u0 }
        )
        (var-set project-nonce new-project-id)
        (ok new-project-id)
    )
)

;; Donate to a project (funds held in escrow)
(define-public (donate (project-id uint) (amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (current-balance (get balance (get-project-balance project-id)))
            (existing-donation (default-to { amount: u0, timestamp: u0 } 
                (map-get? donations { donor: tx-sender, project-id: project-id })))
        )
        (asserts! (get active project) err-not-found)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer STX to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Update project balance
        (map-set project-balances
            { project-id: project-id }
            { balance: (+ current-balance amount) }
        )
        
        ;; Update donation record
        (map-set donations
            { donor: tx-sender, project-id: project-id }
            {
                amount: (+ (get amount existing-donation) amount),
                timestamp: block-height
            }
        )
        
        ;; Update project total raised
        (map-set projects
            { project-id: project-id }
            (merge project { total-raised: (+ (get total-raised project) amount) })
        )
        
        (ok true)
    )
)

;; Create a milestone for a project
(define-public (create-milestone (project-id uint) (description (string-ascii 200)) (target-amount uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
            (new-milestone-id (+ (var-get milestone-nonce) u1))
        )
        (asserts! (is-eq tx-sender (get owner project)) err-unauthorized)
        (asserts! (get active project) err-not-found)
        (asserts! (> target-amount u0) err-invalid-amount)
        
        (map-set milestones
            { milestone-id: new-milestone-id }
            {
                project-id: project-id,
                description: description,
                target-amount: target-amount,
                released: false,
                verified: false,
                verification-count: u0,
                created-at: block-height
            }
        )
        (var-set milestone-nonce new-milestone-id)
        (ok new-milestone-id)
    )
)

;; Impact Weaver verifies a milestone
(define-public (verify-milestone (milestone-id uint))
    (let
        (
            (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-not-found))
            (weaver (unwrap! (map-get? impact-weavers { weaver: tx-sender }) err-unauthorized))
            (already-verified (default-to { verified: false } 
                (map-get? milestone-verifications { milestone-id: milestone-id, weaver: tx-sender })))
        )
        (asserts! (get active weaver) err-unauthorized)
        (asserts! (not (get verified already-verified)) err-already-exists)
        (asserts! (not (get released milestone)) err-invalid-milestone)
        
        ;; Record verification
        (map-set milestone-verifications
            { milestone-id: milestone-id, weaver: tx-sender }
            { verified: true }
        )
        
        ;; Update milestone verification count
        (map-set milestones
            { milestone-id: milestone-id }
            (merge milestone { 
                verification-count: (+ (get verification-count milestone) u1)
            })
        )
        
        ;; Update weaver stats
        (map-set impact-weavers
            { weaver: tx-sender }
            (merge weaver {
                validations: (+ (get validations weaver) u1),
                reputation: (+ (get reputation weaver) u1)
            })
        )
        
        (ok true)
    )
)

;; Release funds for a verified milestone
(define-public (release-milestone-funds (milestone-id uint))
    (let
        (
            (milestone (unwrap! (map-get? milestones { milestone-id: milestone-id }) err-not-found))
            (project (unwrap! (map-get? projects { project-id: (get project-id milestone) }) err-not-found))
            (project-balance (get balance (get-project-balance (get project-id milestone))))
        )
        (asserts! (is-eq tx-sender (get owner project)) err-unauthorized)
        (asserts! (not (get released milestone)) err-invalid-milestone)
        (asserts! (>= (get verification-count milestone) u3) err-milestone-not-ready)
        (asserts! (>= project-balance (get target-amount milestone)) err-insufficient-funds)
        
        ;; Transfer funds from escrow to project owner
        (try! (as-contract (stx-transfer? (get target-amount milestone) tx-sender (get owner project))))
        
        ;; Update milestone status
        (map-set milestones
            { milestone-id: milestone-id }
            (merge milestone { 
                released: true,
                verified: true
            })
        )
        
        ;; Update project balance
        (map-set project-balances
            { project-id: (get project-id milestone) }
            { balance: (- project-balance (get target-amount milestone)) }
        )
        
        ;; Update project total released
        (map-set projects
            { project-id: (get project-id milestone) }
            (merge project { 
                total-released: (+ (get total-released project) (get target-amount milestone)),
                impact-score: (+ (get impact-score project) u10)
            })
        )
        
        (ok true)
    )
)

;; Stake to become an Impact Weaver
(define-public (stake-as-weaver (amount uint))
    (let
        (
            (existing-weaver (default-to 
                { stake-amount: u0, validations: u0, reputation: u0, active: false }
                (map-get? impact-weavers { weaver: tx-sender })))
        )
        (asserts! (>= amount min-stake-amount) err-insufficient-stake)
        
        ;; Transfer stake to contract
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        
        ;; Register or update weaver
        (map-set impact-weavers
            { weaver: tx-sender }
            {
                stake-amount: (+ (get stake-amount existing-weaver) amount),
                validations: (get validations existing-weaver),
                reputation: (get reputation existing-weaver),
                active: true
            }
        )
        
        (ok true)
    )
)

;; Unstake and withdraw
(define-public (unstake-weaver (amount uint))
    (let
        (
            (weaver (unwrap! (map-get? impact-weavers { weaver: tx-sender }) err-not-found))
        )
        (asserts! (>= (get stake-amount weaver) amount) err-insufficient-funds)
        
        ;; Transfer stake back to weaver
        (try! (as-contract (stx-transfer? amount tx-sender tx-sender)))
        
        ;; Update weaver stake
        (let
            (
                (new-stake (- (get stake-amount weaver) amount))
            )
            (map-set impact-weavers
                { weaver: tx-sender }
                (merge weaver {
                    stake-amount: new-stake,
                    active: (>= new-stake min-stake-amount)
                })
            )
        )
        
        (ok true)
    )
)

;; Create impact cluster relationship
(define-public (create-cluster-link (parent-project uint) (child-project uint))
    (let
        (
            (parent (unwrap! (map-get? projects { project-id: parent-project }) err-not-found))
            (child (unwrap! (map-get? projects { project-id: child-project }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner parent)) err-unauthorized)
        (asserts! (get active parent) err-not-found)
        (asserts! (get active child) err-not-found)
        
        (map-set impact-clusters
            { parent-project: parent-project, child-project: child-project }
            { active: true }
        )
        
        (ok true)
    )
)

;; Apply ripple multiplier to boost related project
(define-public (apply-ripple-boost (from-project uint) (to-project uint))
    (let
        (
            (from (unwrap! (map-get? projects { project-id: from-project }) err-not-found))
            (to (unwrap! (map-get? projects { project-id: to-project }) err-not-found))
            (cluster (unwrap! (map-get? impact-clusters 
                { parent-project: from-project, child-project: to-project }) err-not-found))
            (boost-amount (/ (* (get impact-score from) ripple-multiplier) u10000))
        )
        (asserts! (get active cluster) err-not-found)
        (asserts! (> (get impact-score from) u0) err-invalid-amount)
        
        ;; Boost child project impact score
        (map-set projects
            { project-id: to-project }
            (merge to {
                impact-score: (+ (get impact-score to) boost-amount)
            })
        )
        
        (ok boost-amount)
    )
)

;; Update project impact score (owner only for admin purposes)
(define-public (update-impact-score (project-id uint) (new-score uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        
        (map-set projects
            { project-id: project-id }
            (merge project { impact-score: new-score })
        )
        
        (ok true)
    )
)

;; Deactivate a project
(define-public (deactivate-project (project-id uint))
    (let
        (
            (project (unwrap! (map-get? projects { project-id: project-id }) err-not-found))
        )
        (asserts! (is-eq tx-sender (get owner project)) err-unauthorized)
        
        (map-set projects
            { project-id: project-id }
            (merge project { active: false })
        )
        
        (ok true)
    )
)
