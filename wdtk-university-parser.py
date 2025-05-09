import pandas as pd
import feedparser
import time
import re
import os
from urllib.parse import quote

output_dir = "wdtk-data"
if not os.path.exists(output_dir):
    os.makedirs(output_dir)
    print(f"Created directory: {output_dir}")

print("Downloading all-authorities.csv from whatdotheyknow.com...")
authorities_url = "https://www.whatdotheyknow.com/body/all-authorities.csv"
df = pd.read_csv(authorities_url)

print("Filtering for university authorities...")
df = df[df['Tags'].str.contains('university', case=False, na=False)]
print(f"Found {len(df)} university authorities.")

for i, row in df.iterrows():
    url_name = row.get('URL name')
    if pd.isna(url_name):
        print(f"Row {i} has no URL name, skipping.")
        continue

    # Build the feed URL
    encoded_name = quote(str(url_name), safe='')
    feed_url = f"https://www.whatdotheyknow.com/feed/body/{encoded_name}/"
    print(f"Processing URL: {feed_url}")

    try:
        print("Attempting to fetch and parse RSS feed...")
        parsed = feedparser.parse(feed_url)

        # Check for entries
        if not parsed.feed or not parsed.entries:
            print(f"No items found for {url_name}")
        else:
            # Extract relevant fields
            items = []
            for entry in parsed.entries:
                title = entry.get('title', '')
                body = entry.get('description', '')
                # Some feeds use 'published', others use 'updated'
                date = entry.get('published', entry.get('updated', ''))
                items.append({'title': title, 'body': body, 'date': date})

            feed_df = pd.DataFrame(items)

            # Sanitize filename root
            safe_root = re.sub(r'[^A-Za-z0-9_.-]', '_', str(url_name))
            if not safe_root:
                safe_root = f"feed_data_{i}"
            output_file = os.path.join(output_dir, f"{safe_root}.csv")

            # Save to CSV
            feed_df.to_csv(output_file, index=False)
            print(f"Saved data for {url_name} to {output_file}")

    except Exception as e:
        print(f"Error processing feed for {url_name}: {e}")

    # Pause between requests
    time.sleep(0.3)
