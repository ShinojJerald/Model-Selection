# TuneKadal Home Page Design Concept

## Goal

Design a premium, cinematic, Netflix-style home page for **TuneKadal**, a Tamil-first live radio app. The home page should feel curated, trustworthy, and hand-designed rather than generic or AI-generated.

The interface should communicate:

- Tamil global identity
- Premium audio discovery
- Fast live listening
- No intrusive ad clutter
- Clean categorization by country, artist-style radio, mood, and favorites

A static HTML prototype is available at [`prototypes/tunekadal-home.html`](prototypes/tunekadal-home.html).

---

## Brand Direction

### App name

**TuneKadal**

### Meaning

The name suggests an ocean of tunes, which works well for a global Tamil radio app because Tamil stations come from India, Sri Lanka, Malaysia, Singapore, UAE, Australia, New Zealand, the USA, and other diaspora communities.

### Brand personality

- Premium, not flashy
- Warm, not childish
- Tamil-first, but globally polished
- Cinematic, but still fast and functional
- Minimal, but not empty

### Avoid

- Stock-looking AI portraits
- Overly glossy fake 3D icons
- Random neon gradients everywhere
- Too many emojis inside station names
- Crowded red/yellow-on-black layouts
- Banner-ad-style spacing
- Generic music app layouts that could belong to any language

---

## Visual Style

### Overall theme

Use a dark cinematic interface with soft glass layers, warm gold highlights, ocean-inspired teal/blue accents, and carefully controlled gradients.

This gives TuneKadal a premium entertainment feel without copying Netflix directly.

### Color palette

| Role | Color | Usage |
|---|---:|---|
| App background | `#05070d` | Main dark canvas |
| Elevated surface | `#0c1019` | Cards, bottom nav, mini-player |
| Primary text | `#f7f3ea` | Titles and important labels |
| Muted text | `#a9adb8` | Metadata and secondary labels |
| Premium gold | `#ffd36a` | Active states, links, premium accents |
| Coral | `#ff5e48` | Live energy, highlights |
| Ocean teal | `#22d6c6` | TuneKadal identity accent |
| Deep blue | `#5f8cff` | Ocean/radio depth accent |
| Purple | `#a46bff` | Night listening and entertainment accent |

### Typography

Use a modern system sans stack for the prototype. In production, choose a typeface that supports Tamil beautifully.

Recommended production options:

- English/UI: Inter, SF Pro, or similar modern sans
- Tamil: Noto Sans Tamil, Noto Serif Tamil for select editorial moments, or another high-quality Tamil font

### Design details that make it feel less AI-generated

- Use consistent spacing tokens instead of random placement.
- Use real product states such as “Live from Chennai,” “Continue listening,” and “Playing now.”
- Avoid fake artist photography unless licensed.
- Use abstract station art, monograms, and gradients until proper station logos are approved.
- Keep a clear hierarchy: brand, filters, hero, rails, countries, mini-player, nav.
- Include realistic metadata such as country, station type, and bitrate.

---

## Home Page Structure

### 1. Status and header

The header uses:

- TuneKadal brand mark
- Small premium descriptor: “Tamil radio vault”
- Search action
- Profile/settings action
- Quick filter chips

Recommended chips:

- For You
- Live FM
- Countries
- Artists
- Devotional

### 2. Hero feature

The hero is the emotional anchor of the page. It should feel like a streaming-service feature card, but for live radio.

Example hero:

- Badge: `LIVE FROM CHENNAI`
- Title: `Evening Tamil Melody`
- Description: `Handpicked live stations for the drive home — smooth voices, classic hits, and zero visual clutter.`
- Primary CTA: `Listen Now`
- Secondary CTA: `My List`

### 3. Continue Listening rail

This row makes the app feel personal immediately.

Example cards:

- Madras Melody
- Raja Classics
- Tamil 90s Gold

Each card should include:

- Square artwork
- Station/category name
- Metadata such as city, type, or stream quality

### 4. Tamil Around the World

This section supports the core differentiation: global Tamil radio.

Example country cards:

- Malaysia
- Singapore
- Sri Lanka
- UAE

Each country card can show the number of currently online stations.

### 5. Artist Radio rail

Artist radio should be presented carefully as curated stream discovery, not as official artist-owned content unless permission exists.

Example cards:

- Rahman Vibes
- SPB Classics
- Pradeep Mood

Use monograms or abstract visuals until proper licensed artwork is available.

### 6. Mini-player

The mini-player should be persistent and polished.

It should show:

- Current station artwork
- Station name
- Now-playing text or fallback metadata
- Play/pause button

### 7. Bottom navigation

Recommended tabs:

- Home
- Live
- Saved
- More

Keep the nav minimal. Do not overload the first version with too many tabs.

---

## Prototype Notes

The included HTML prototype intentionally uses:

- No external images
- No copyrighted station logos
- No celebrity photos
- No AI-generated portraits
- CSS-only abstract artwork
- Realistic station/category naming
- Premium mobile spacing and hierarchy

This approach keeps the design safer while still looking polished. Once the station catalog is approved, individual station logos can replace the placeholder monograms.

---

## Product Design Rules for TuneKadal

### Rule 1: Radio should start fast

The home page should never block users from starting audio. The hero CTA and recently played stations should be reachable immediately.

### Rule 2: The app should not feel like an ad container

Avoid banner-shaped white blocks, cluttered ad slots, and dense list rows. The premium promise depends on visual calm.

### Rule 3: Use Tamil identity with restraint

Tamil language, country grouping, and cultural relevance matter more than decorative overload. A clean Tamil-first product will feel more premium than a busy one.

### Rule 4: Every station card needs useful metadata

Cards should answer:

- What is this?
- Where is it from?
- Is it live?
- Why should I tap it?

### Rule 5: Use safe artwork first

Until permissions are confirmed, use:

- Letter marks
- Abstract gradient covers
- Country cards
- Licensed illustrations
- Official station logos only when permitted

---

## Recommended Next Design Steps

1. Convert the static HTML concept into Figma screens.
2. Create a design system with colors, typography, cards, chips, rails, mini-player, and bottom navigation.
3. Design the full now-playing screen.
4. Design country detail pages.
5. Design station detail pages.
6. Design empty/error states for broken streams.
7. Design onboarding around “Tamil radio around the world.”
8. Test the home page with Tamil users before writing the full app.

