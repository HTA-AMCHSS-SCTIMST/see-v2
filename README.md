# AMCHSS · SCTIMST Structured Expert Elicitation Platform

**Achutha Menon Centre for Health Science Studies (AMCHSS)**  
**Sree Chitra Tirunal Institute for Medical Sciences & Technology (SCTIMST), Trivandrum**  

A digital, multi-round **Structured Expert Elicitation Platform** implementing the **Sheffield Elicitation Framework (SHELF v4)** within a **Modified Delphi** protocol for Health Technology Assessment (HTA) and clinical decision support.

The platform quantifies subjective parameter uncertainty for health economic models and clinical trials when empirical registry or trial data are scarce, incomplete, or conflicting.

---

## Key Methodological Features

* **Formal SHELF Implementation:** Mathematical fitting of continuous probability distributions (**Beta, Normal, Log-Normal, Gamma, and Log-Student-t**) to elicited expert quantiles using least-squares optimization.
* **Mathematical Linear Pooling:** Continuous pooled group consensus density $f_{\text{pool}}(\theta) = \sum w_i f_i(\theta)$ producing the true group consensus curve, group true mean, and pooled credible intervals ($P_{10}, P_{50}, P_{90}$).
* **Multi-Round Delphi Protocol:**
  * **Round 1 (Independent Judgments):** New experts always start in Round 1, providing uninfluenced baseline judgments. Responses are locked upon submission.
  * **Facilitator Deliberation Gate:** Facilitator reviews group fits, true mean estimates, and peer rationales to decide whether consensus has been reached or if Delphi Round 2 is needed.
  * **Round 2 (Deliberation & Re-elicitation):** Experts engage in blinded peer review (viewing their personal Round 1 numbers vs. group consensus and peer rationales) and re-assess parameters with **pre-filled Round 1 baselines**.
* **Streamlined IEC Compliance:** SCTIMST Institutional Ethics Committee compliant digital consent barrier; strict participant de-identification (*Expert A, Expert B, Expert C...*).
* **Dual Elicitation Modalities:**
  * **Low-High-Best Quantiles ($P_{10} / P_{50} / P_{90}$):** Strict monotonicity enforcement ($L \le P_{10} < P_{50} < P_{90} \le U$).
  * **Chips-and-Bins (Roulette):** 20 chips interactively allocated across 10 dynamically scaled histogram bins for skewed variables.
* **Evidence Dossier Export:** Downloadable HTA evidence synthesis dossiers in CSV, audit PDF, and Quarto report formats.

---

## Core Technology Stack

| Layer | Technologies | Role / Architecture |
|---|---|---|
| **Front-End & UI** | R Shiny, htmltools, CSS3, JavaScript | Reactive wizard flow, step indicators, AMCHSS-SCTIMST clinical design system. |
| **Interactive Visuals** | Plotly, ggplot2, `www/chips.js` | Dynamic PDF/CDF density charts, consensus curves, and interactive chip grid. |
| **Statistical Engine** | Custom Oakley SHELF & CRAN SHELF | Non-linear optimization, cumulative probability integration, linear opinion pooling. |
| **Database & Persistence** | PostgreSQL / Supabase (`JSONB`) | Relational storage for studies, parameters, versioned judgments, and immutable audit logs. |
| **Authentication & Tokens** | 256-bit Cryptographic Tokens | Dynamic origin link generation (`/?study=<slug>&t=<token>`), session auto-healing. |
| **Reporting & Synthesis** | Quarto, rmarkdown, knitr | Publication-ready HTA audit dossiers, parameter tables, and mathematical reports. |

---

## End-to-End Delphi Workflow

```
                               ROUND 1: INDEPENDENT ELICITATION
┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐     ┌──────────────┐
│   Welcome    │ ──▶ │ Participant  │ ──▶ │ Practice     │ ──▶ │ Plausible    │ ──▶ │ Question     │
│   Briefing   │     │ Consent (IEC)│     │ Calibration  │     │ Bounds (L, U)│     │ Elicitation  │
└──────────────┘     └──────────────┘     └──────────────┘     └──────────────┘     └──────┬───────┘
                                                                                           │
                                                                                           ▼
                                                                                    ┌──────────────┐
                                                                                    │ Submit & Lock│
                                                                                    │ Round 1 Data │
                                                                                    └──────┬───────┘
                                                                                           │
                     FACILITATOR REVIEW & DELIBERATION GATE                                │
┌──────────────────────────────────────────────────────────────────────────────────────────┘
│
▼
┌───────────────────────────────┐
│ Facilitator Deliberation View │ ──▶ Computes SHELF Linear Pool & Group True Mean
│ Reviews Consensus & Rationales│
└──────────────┬────────────────┘
               │
               ├───────────────────────────────────────────────┐
               ▼ (Consensus Not Met)                           ▼ (Consensus Achieved)
┌───────────────────────────────┐             ┌────────────────────────────────┐
│   Initiate Delphi Round 2     │             │   Mark Elicitation Complete    │
└──────────────┬────────────────┘             └────────────────┬───────────────┘
               │                                               │
               ▼                                               ▼
  ROUND 2: DELIBERATION & REVISION              FINAL HTA EVIDENCE SYNTHESIS
┌───────────────────────────────┐             ┌────────────────────────────────┐
│ 1. Delphi Round 2 Briefing    │             │ • Export Full Callset (CSV)    │
│ 2. Blinded Peer Review        │             │ • Download Audit PDF           │
│    (Personal R1 vs Group Pool)│             │ • Generate Quarto Dossier      │
│ 3. Pre-filled Re-elicitation  │             │ • Integrate into HTA Model     │
│ 4. Final Lock & Confirmation  │             │   (Markov / PSA Simulation)    │
└───────────────────────────────┘             └────────────────────────────────┘
```

---

## Database (Supabase / PostgreSQL) Setup

The platform uses **PostgreSQL** with high-performance `JSONB` document storage:

1. Create a project at [supabase.com](https://supabase.com).
2. Navigate to **Project Settings** -> **Database** -> **Connection string**.
3. Select **URI** and choose **Mode: Transaction** (port `6543`).
4. Copy the connection URI:
   ```
   postgresql://postgres.[PROJECT-REF]:[PASSWORD]@aws-0-[REGION].pooler.supabase.com:6543/postgres
   ```
5. *(Optional)* Run [`schema.sql`](schema.sql) in the Supabase **SQL Editor**. If omitted, the application will automatically initialize the necessary tables, columns, and GIN indexes upon first startup.

---

## Local Setup (Developer)

### 1. Requirements
* R (version 4.2 or newer).
* R packages: `shiny`, `htmltools`, `DBI`, `RPostgres`, `jsonlite`, `plotly`, `ggplot2`, `SHELF`, `rmarkdown`, `knitr`.

### 2. Configure `.Renviron`
Copy `.Renviron.example` to `.Renviron` in the project root:
```bash
cp .Renviron.example .Renviron
```
Edit `.Renviron` with your database credentials:
```env
SUPABASE_DB_URL=postgresql://postgres.USER:PASSWORD@aws-0-REGION.pooler.supabase.com:6543/postgres
AUTH_DEV_MODE=true
APP_ROLE=both
SURVEY_PUBLIC_URL=
SESSION_TIMEOUT_MINUTES=30
```
> [!NOTE]
> `SURVEY_PUBLIC_URL` can be left blank for local development. The platform automatically resolves URLs dynamically based on `window.location.origin` in the user's browser.

### 3. Launch Application
Run in terminal:
```bash
Rscript -e 'shiny::runApp(port = 3938)'
```
Or in R / RStudio:
```r
source("run_local.R")
```
Access the application at `http://127.0.0.1:3938`.

---

## Deployment to shinyapps.io

1. **Configure rsconnect:**
   ```r
   rsconnect::setAccountInfo(
     name   = "<YOUR_ACCOUNT_NAME>",
     token  = "<YOUR_TOKEN>",
     secret = "<YOUR_SECRET>"
   )
   ```
2. **Deploy with Deployment Script:**
   ```bash
   Rscript deploy/shinyapps.R
   ```
   Or inside R:
   ```r
   source("deploy/shinyapps.R")
   ```

---

## User Roles & Access Control

* **Admin:** System monitoring, organization management, and user provisioning.
* **Facilitator / Study Manager:** Formulates clinical quantities of interest (QoIs), sets plausible bounds $[L, U]$, invites clinical experts, manages Delphi rounds, evaluates SHELF consensus, and exports audit dossiers.
* **Expert Participant:** Receives a secure tokenized link (`/?study=<slug>&t=<token>`). Bypasses passwords, completes consent, provides judgments, and participates in Round 2 deliberation.
* **Viewer:** Read-only access to view completed HTA case studies and finalized group distributions.

---

## Governance & Ethics

* **Compliance:** Aligned with SCTIMST Institutional Ethics Committee (IEC) guidelines for research involving expert panels.
* **Non-Coaching Facilitator Standard:** Facilitators are bound by neutrality protocols preventing subjective steering of expert judgment.
* **Confidentiality:** All participant distributions and rationales are de-identified in peer reviews.
* **Auditability:** Every judgment, timestamp, parameter limit, and facilitator action is immutably logged for regulatory and journal peer-review scrutiny.

---
*Maintained by Achutha Menon Centre for Health Science Studies (AMCHSS), SCTIMST Trivandrum.*