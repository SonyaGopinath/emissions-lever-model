# SCOPE: Sustainability Cost & Output Projection Engine

**SCOPE** is an interactive R Shiny application that simulates the financial and environmental impact of sustainability decisions made by firms. It bridges sustainability strategy with business outcomes, helping users understand the downstream effects of emissions, renewable energy use, and carbon pricing on firm output, revenue, and profit.

---

## 🔍 What This Model Does

SCOPE allows users to:

- Input custom baseline data (production, revenue, emissions).
- Simulate emissions and financial projections over a 10-year horizon.
- Adjust levers like **carbon pricing** and **renewable energy share**.
- Observe how emissions-related decisions affect **profitability** over time.

The app helps users identify:
- How emissions intensity evolves.
- When sustainability failures begin eroding output and revenue.
- The business case for proactive environmental investments.

---

## 🎛 Key Inputs

| Input                          | Description |
|-------------------------------|-------------|
| Initial Output                | Tonnes of goods/services produced |
| Initial Revenue               | Starting revenue in £ |
| Initial Emissions             | Current emissions in tonnes CO2e |
| Firm Size                     | Worker base (Small, Mid-size, Large) |
| Capital Productivity Rate     | Annual output growth from capital investment (%) |
| Carbon Price                  | Cost of carbon per tonne emitted (£) |
| Labour Productivity Drop      | Emissions-driven decline in productivity (%) |
| Planned Renewable Energy Share| % of energy planned from RE sources |
| Emissions Intensity Worsening| Optional toggle to simulate degradation over time |

---

## 📈 Outputs

Each scenario produces:

- **Emissions graph**: Total emissions projected over 10 years.
- **Output graph**: BAU vs adjusted production paths.
- **Revenue graph**: BAU vs emissions-adjusted revenues.
- **Profit graph**: Traditional vs carbon-adjusted profitability.
- **Snapshot table**: Key performance indicators in Year 1 vs Year 10.

---

## ⚙️ How to Use

1. Clone this repo or launch via [shinyapps.io link].
2. Input baseline data and adjust relevant levers.
3. Toggle emissions intensity degradation if relevant.
4. Review emissions and financial outputs.
5. Use snapshot tab for quick executive insight.

---

## 📌 Notes & Assumptions

- Simplified relationships. No lag effects or sector multipliers (yet).
- Carbon costs apply from Year 1 at full intensity (for model clarity).
- Renewable Energy Share impacts emissions intensity linearly.
- Labour productivity loss kicks in from a user-defined year.
- All monetary outputs are illustrative and not real market valuations.

---

## 🚧 Roadmap

Future enhancements will include:

- Sector-specific benchmarks and carbon thresholds
- Time-lagged effects and inter-variable feedbacks
- Interactive cost-of-inaction scenarios
- Integration with emissions benchmarks (e.g., ISO 14097, TCFD)

---

## 👤 Author

Built by **Sonya**  
Designed to support evidence-based sustainability planning and communicate the real cost of inaction.

