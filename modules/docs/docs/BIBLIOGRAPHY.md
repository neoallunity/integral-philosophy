# Bibliography Management System

## Overview

The Integral Philosophy Publishing System includes a comprehensive bibliography management system that automatically extracts citations from LaTeX articles and generates bibliography files. This system handles Russian academic articles and integrates with the LaTeX publishing pipeline.

## Features

### 🔍 **Automatic Citation Detection**
- Detects multiple LaTeX citation formats: `\cite{}`, `\citet{}`, `\citep{}`, `\footcite{}`
- Handles complex citation patterns: `\cite[p.12]{author}`
- Supports multiple citations per command: `\cite{author1,author2}`

### 📚 **Context-Aware Entry Generation**
- Analyzes text around citations to extract authors, years, and titles
- Identifies publication context from article content
- Creates properly formatted BibTeX entries

### 🗂️ **Multiple Bibliography Formats**
- Individual bibliography files per article (`articles/*/references.bib`)
- Master bibliography (`global-bibliography.bib`)
- Automatic synchronization between individual and master bibliographies

### 🌐 **Russian Academic Support**
- Handles Russian author names and publication titles
- Supports Russian academic citation styles
- Processes Unicode characters in LaTeX files

## Usage

### Extract Citations from All Articles
```bash
python3 scripts/extract_bibliography.py extract
```

### Process Single Article
```bash
python3 scripts/extract_bibliography.py extract --article safonov
```

### Update Master Bibliography
```bash
python3 scripts/extract_bibliography.py update-master
```

### Full Processing Cycle
```bash
python3 scripts/extract_bibliography.py all
```

## Integration with LaTeX

### Article Structure
Each article should include:
```latex
% In main.tex
\bibliography{references}
\bibliographystyle{plain}
```

### Citation Examples
```latex
% Simple citation
\cite{goethe1957}

% Multiple citations
\cite{moiseev1999,moiseev2024a}

% Citation with page reference
\cite[p.25]{jung1968}

% Textual citation
\textcite{wilber2000}

% Footnote citation
\footcite{grof1988}
```

## File Structure

```
Magazine/
├── scripts/
│   └── extract_bibliography.py     # Main extraction script
├── articles/
│   ├── article1/
│   │   ├── main.tex               # Article with citations
│   │   └── references.bib         # Generated bibliography
│   └── article2/
│       ├── main.tex
│       └── references.bib
└── global-bibliography.bib         # Master bibliography
```

## Bibliography Entry Types

The system automatically determines entry types:

### Book Entries
```bibtex
@book{goethe1957,
    author = {Goethe, Johann Wolfgang von},
    title = {Избранные произведения по естествознанию},
    year = {1957},
    publisher = {Издательство Академии наук СССР}
}
```

### Article Entries
```bibtex
@article{moiseev2024a,
    author = {Моисеев, В. И.},
    title = {Русская философия всеединства как прообраз интегральной науки},
    year = {2024},
    journal = {Соловьевские исследования},
    volume = {3},
    number = {83},
    pages = {38--48}
}
```

### Miscellaneous Entries
```bibtex
@misc{unpublished,
    author = {Author Name},
    title = {Unpublished Work},
    year = {n.d.},
    note = {Unpublished manuscript}
}
```

## Auto-Generated Entries

When context analysis finds limited information, the system creates basic entries:

```bibtex
@misc{citation_key,
    author = {Citation Key},
    title = {Work Title},
    year = {Year},
    note = {Auto-generated entry from article context}
}
```

## Context Analysis Features

### Author Recognition
- Detects author names in text
- Handles Russian name variations
- Matches with master bibliography entries

### Year Extraction
- Finds publication years in brackets: `[2024]`
- Detects years in parentheses: `(2024)`
- Handles Russian year formats

### Title Detection
- Extracts titles from quotes: `"Название работы"`
- Handles Russian quotes: `«Название»`
- Identifies work titles near author mentions

## Master Bibliography

The `global-bibliography.bib` file contains:
- Classical philosophical works
- Standard academic references
- Auto-generated entries from all articles
- Duplicates automatically removed

## Processing Pipeline

1. **Text Extraction**: Reads LaTeX files and extracts citation commands
2. **Citation Parsing**: Identifies citation keys and formats
3. **Context Analysis**: Scans surrounding text for bibliographic information
4. **Entry Generation**: Creates BibTeX entries with proper formatting
5. **File Writing**: Generates individual bibliography files
6. **Master Update**: Synchronizes with master bibliography

## Error Handling

The system provides detailed feedback:
- ✅ Successful operations
- ⚠️ Warnings for missing information
- ❌ Errors for file access problems
- ℹ️ Information messages

## Integration with Doxygen

The bibliography system integrates with the documentation pipeline:
- Processes LaTeX files alongside code
- Includes bibliography files in documentation
- Maintains cross-references between articles

## Best Practices

### Adding Citations to Articles
1. Use consistent citation keys: `authoryear` format
2. Include proper LaTeX citation commands
3. Add context information around citations
4. Use author-date conventions for keys

### Maintaining Bibliographies
1. Run extractor after adding new citations
2. Review auto-generated entries
3. Update master bibliography periodically
4. Check for duplicate entries

### File Organization
1. Keep one bibliography per article
2. Use descriptive citation keys
3. Include publication details in entries
4. Maintain master bibliography consistency

## Troubleshooting

### No Citations Found
- Check LaTeX citation syntax
- Verify citation command formats
- Ensure proper bracket usage

### Missing Entries
- Check master bibliography for existing entries
- Verify context analysis results
- Manually add missing information

### Encoding Issues
- Ensure UTF-8 encoding for LaTeX files
- Check Russian character support
- Verify BibTeX file encoding

## Future Enhancements

Planned improvements to the bibliography system:

### Advanced Context Analysis
- Machine learning for author identification
- Improved title extraction algorithms
- Cross-reference validation

### Integration Features
- Direct BibTeX database integration
- Online bibliography service connections
- Automatic DOI resolution

### Quality Assurance
- Duplicate detection and removal
- Citation format validation
- Bibliography consistency checking

## Examples

### Complete Article with Citations
```latex
\documentclass{article}
\begin{document}

% Text with citations
Данный подход во многом опирается на естественнонаучную методологию Гете \cite{goethe1957}.

% Multiple citations
Работы К. Юнга \cite{jung1968}, С. Грофа \cite{grof1988} и К. Уилбера \cite{wilber2000} повлияли на этот подход.

% Citation with page reference
Согласно теории мироподобных систем \cite[p.45]{moiseev2022}...

\bibliography{references}
\bibliographystyle{plain}
\end{document}
```

### Generated Bibliography Output
```bibtex
% Bibliography for article
% Auto-generated by extract_bibliography.py

@book{goethe1957,
    author = {Goethe, Johann Wolfgang von},
    title = {Избранные произведения по естествознанию},
    year = {1957},
    publisher = {Издательство Академии наук СССР}
}

@book{jung1968,
    author = {Jung, Carl Gustav},
    title = {Man and His Symbols},
    year = {1968},
    publisher = {Doubleday}
}

@book{grof1988,
    author = {Grof, Stanislav},
    title = {За пределами мозга: Рождение, смерть и трансцендентность в психотерапии},
    year = {1988},
    publisher = {ЭТЦ Либрис}
}

@book{wilber2000,
    author = {Wilber, Ken},
    title = {Integral Psychology: Consciousness, Spirit, Psychology, Therapy},
    year = {2000},
    publisher = {Shambhala Publications}
}

@book{moiseev2022,
    author = {Моисеев, В. И.},
    title = {Теория мироподобных систем},
    year = {2022},
    publisher = {Перо}
}
```

This comprehensive bibliography management system ensures academic rigor and consistency across the Integral Philosophy Publishing System while supporting both Russian and international academic standards.