# ETH Zurich — Data Analysis for Public Policy 
### *Autumn 2026*
> *From data to policy-relevant insights using R.*

---

## 🔗 Quick links

- 📅 **Schedule:** see Syllabus (every Wednesday 14:00-16:00)
- 🧾 **Syllabus (PDF):** [`/generalFiles/syllabus.pdf`](./generalFiles/syllabus.pdf) 
- 📦 **Datasets:** [`/ESSData`](./ESSData)  

<!-- 🧪 **Assignments:** [`/assignments`](./assignments) -->
<!-- 📊 **Extra materials:** [`/extra`](./extra) -->

---

## ℹ️ About the course

This course introduces students to the necessary fundamentals of mathematics and statistics, and their applications, to conduct quantitative policy evaluations.
The course will provide a survey of theoretical foundational concepts and techniques. The applied part
of the course will focus on implementing these techniques in R, as well as developing the
practical skills in the language required to be able to independently conduct basic data analysis in research projects.

**Instructors:**  
Dr E. Keith Smith ([esmith@ethz.ch](mailto:esmith@ethz.ch))  
Camille Fournier de Lauriere ([cfournier@ethz.ch](mailto:cfournier@ethz.ch))

---

## 🎯 Learning objectives for the R practice.

1. How to use R and RStudio: General knowledge of the R language.
2. Implementing examples from the course: all content covered in the theoretical sections will be
exemplified in R.
3. Data handling fundamentals: how to work in the tidy ecosystem (tidyverse).
4. Data visualization fundamentals: learning about most common plots and how to implement them in R,
mainly using ggplot2.

---

## ⚙️ Setup

Please download and install R and RStudio before the first class.

---

## 📁 Repository Organization

The repository follows the structure of the course and is organized to help you navigate the lecture materials, datasets, and other files efficiently.

### Main folders

- **`01_introClass/`, `02_descriptives/`, `04_samplingDistributions/`, `05_hypothesisTesting/`, …**  
  These numbered folders correspond to lecture topics in chronological order.  
  Each contains the R scripts used in class and any potentially small datasets specific to that session.  
  Work through them sequentially, they build upon each other.

- **`ESSData/`**  
  Contains the datasets drawn from the *European Social Survey (ESS)*.  
  Includes CSV files for different countries and survey waves (8–10), along with their HTML codebooks.  

- **`generalFiles/`**  
  Contains reference materials such as the syllabus, formula sheet, and R cheatsheets (located in the `/cheatsheets/` subfolder).

---

### Structure of each class script

Each numbered folder's `.R` script follows the same five sections, in order:

1. **`learnings today`** — a short bullet list of what the session covers, so you know what to expect before diving in.
2. **`setup`** — packages to load (e.g. `tidyverse`) and any general notes for the session.
3. **`toolbox`** — the core content: functions, concepts, and worked demonstrations introduced in class, illustrated with simulations and plots.
4. **`exercices`** — worked exercises (often from the course book) with the full solution/code included, meant to be run and discussed in class.
5. **`exercice (your turn)`** — a similar exercise for you to complete on your own, with the code partly filled in (`___` marks what to fill in) and no solution provided.

You can jump to any section directly in RStudio using the section headers (`##### ------- section name ------- #####`) in the document outline (Ctrl+Shift+O / Cmd+Shift+O).

---

### 📖 Preparing for the class

Please read through the upcoming session's script **before class**. 
In class, we will answer questions about anything that's unclear, but we won't walk through the code line by line: coming prepared lets us spend class time on discussion and the parts you actually find difficult, rather than re-reading code together.
There is also a do-it-yourself exercise (without solution) that uses the functions you should learn to use in the class.  

