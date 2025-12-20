import yaml

with open('metadata.yml') as f:
    meta = yaml.safe_load(f)

output = f"""% AUTO-GENERATED from metadata.yml
\\newcommand{{\\journalissue}}{{{meta['issue']['number']}}}
\\newcommand{{\\journalyear}}{{{meta['issue']['year']}}}
\\newcommand{{\\journalurl}}{{{meta['journal']['url']}}}
"""

with open('cfg/cfg-metadata.tex', 'w') as f:
    f.write(output)

