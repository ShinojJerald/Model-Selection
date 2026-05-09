# Tamil FM Radio App Product Plan

## 1. Product Vision

Build a premium Tamil-first audio app where users can listen to live South Indian/Tamil FM radio stations from around the world. The main focus is live FM and internet radio, with artist-themed radio streams, decade-based stations, devotional streams, instrumental streams, and curated Tamil audio categories as supporting discovery experiences.

The app should feel closer to a modern Netflix-style discovery product than a basic radio list. The goal is to attract users who are frustrated by cluttered interfaces and intrusive ads in existing Tamil radio apps.

### Positioning

> A premium home for Tamil radio around the world.

### Possible taglines

- Tamil radio from every corner of the world.
- Live Tamil FM, beautifully organized.
- From Chennai to Toronto, your Tamil stations in one place.
- No clutter. No noise. Just Tamil radio.
- Global Tamil FM, one tap away.

### Important positioning note

The app should not promise that every station is completely ad-free, because live radio streams may contain ads inserted by the radio station itself. The safer promise is:

> No annoying in-app banner ads or intrusive app ads.

---

## 2. Core App Scope

### Primary focus

- Live Tamil FM radio
- Tamil internet radio stations
- Global Tamil stations categorized by country
- Artist-themed radio streams where public streams already exist
- Decade, mood, devotional, instrumental, news, talk, and local Tamil radio categories

### Initial countries to support

- India
- Sri Lanka
- Malaysia
- Singapore
- United Arab Emirates
- Australia
- New Zealand
- United States
- Canada
- United Kingdom
- Switzerland
- France
- Germany

### Recommended MVP station count

Start with 100 to 300 verified Tamil stations. Quality and reliability are more important than having a large but broken catalog.

---

## 3. Competitive Opportunity

Existing Tamil radio apps appear useful but dated. Based on the reference screenshots, common pain points include:

- Heavy banner ads blocking content
- Crowded screens
- Old-style list UI
- Limited premium feel
- Basic favorites experience
- Dated side menu patterns
- Weak discovery and categorization

### Differentiation strategy

Win by offering:

1. A premium modern design
2. No intrusive in-app ads
3. Faster playback
4. Reliable station health monitoring
5. Country-based and mood-based discovery
6. Strong favorites and recently played experience
7. Sleep timer and background playback
8. Clean Tamil and English UI support
9. Easy report-broken-station flow
10. Station request and station owner claim/removal flow

---

## 4. Content and Licensing Reality

The plan is to use publicly available streams and accept the practical risk. This can work for an MVP, but public availability does not automatically mean the app has commercial redistribution rights.

### Practical risk-reduction rules

Do not:

- Host copyrighted songs on your own server.
- Allow users to download or store songs offline.
- Re-stream station audio through your own servers unless you understand licensing, bandwidth, and rights implications.
- Copy the existing SouthRadios app's curated database, artwork, descriptions, app name, logo, or UI assets.
- Use unlicensed celebrity or artist photos from search engines.
- Claim a station is official unless you have permission.
- Mislead users into believing the app is affiliated with stations or artists.

Do:

- Play direct public stream URLs where possible.
- Attribute station names and station owners.
- Link to official station websites when available.
- Add a station owner claim/removal process.
- Keep source evidence for every stream.
- Respond quickly to takedown requests.
- Prefer official station websites as stream sources.
- Use licensed, generated, abstract, or generic artwork for categories.
- Track permission status for every station.

### Suggested app disclaimer

> This app is an independent radio directory and player. Station names, logos, and streams belong to their respective owners. If you own a station and want to update or remove a listing, contact us at support@example.com.

This disclaimer does not remove legal risk, but it shows good faith and gives station owners a clear path to contact you.

### App Store risk

Apple's App Review Guidelines require apps to respect intellectual property and only include content they created or have permission to use. A radio directory app should be prepared to explain how station streams are sourced, how takedown requests are handled, and why the app is not misrepresenting ownership.

Useful reference: [Apple App Review Guidelines](https://developer.apple.com/app-store/review/guidelines/)

---

## 5. How to Bring Stations Into the App

Use multiple sources instead of relying on one source.

### Option A: Public radio directories

Use public internet radio directories as a seed source. Radio Browser is a useful option because it provides station metadata and stream URLs through an API.

Useful reference: [Radio Browser API Documentation](https://docs.radio-browser.info/)

#### Pros

- Fastest way to build an initial catalog
- Provides stream URLs and metadata
- Helpful for discovering Tamil stations globally
- Useful for MVP imports and validation

#### Cons

- Metadata may be inconsistent
- Logos may be missing or low quality
- Stream URLs may break
- Directory presence does not guarantee content rights
- Manual review is still required

#### Recommendation

Use Radio Browser as a seed input, then import stations into your own backend, manually verify them, clean metadata, categorize them, and publish only approved entries.

### Option B: Official station websites

Many stations expose playable streams on their official websites. These may appear as:

- Direct MP3 streams
- AAC streams
- HLS `.m3u8` streams
- Icecast endpoints
- Shoutcast endpoints
- `.pls` playlists
- `.m3u` playlists
- Embedded web players

#### Pros

- Better attribution
- More accurate metadata
- Easier to link back to station homepage
- Better approval story if Apple asks about sourcing

#### Cons

- Manual work
- Stream URLs can change
- Some sites may prohibit third-party use
- Requires continuous maintenance

### Option C: Direct station partnerships

This is the most sustainable long-term strategy. Contact Tamil stations and ask for permission to list them.

Ask for:

- Official stream URL
- Logo permission
- Station description
- Country, city, and language metadata
- Social links
- Schedule information
- Permission to display the station in the app
- Optional promotional partnership

#### Partner pitch

> We are building a premium Tamil radio discovery app. We do not interrupt your stream with intrusive app ads. We can list your station, display your logo, link to your official page, and help global Tamil listeners discover you.

### Option D: User-submitted stations

Allow users to submit missing stations, but require admin moderation before publishing.

Submission fields:

- Station name
- Country
- City or region
- Language
- Stream URL
- Website URL
- Logo URL
- Category
- User note
- Confirmation that the stream is publicly available

Admin review checklist:

- Does the stream work?
- Is it Tamil or relevant to Tamil listeners?
- Is the station legitimate?
- Does the homepage match the stream?
- Is the station duplicate?
- Is the logo safe to use?
- Is the content appropriate?

---

## 6. Recommended Architecture

Do not hardcode station lists inside the mobile app. Use a backend-managed catalog so stations can be added, removed, fixed, or recategorized without submitting a new app update.

```text
Mobile App
  |
  |-- fetches curated station list
  |-- plays selected stream URL directly
  |
Backend API
  |
  |-- stations
  |-- categories
  |-- countries
  |-- featured rows
  |-- search
  |-- favorites sync
  |-- stream health status
  |
Admin Dashboard
  |
  |-- add/edit station
  |-- approve user submissions
  |-- change categories
  |-- mark station broken
  |-- upload licensed images
  |
Background Worker
  |
  |-- checks streams every few hours
  |-- validates bitrate/codec
  |-- detects dead stations
  |-- parses Icecast/Shoutcast metadata
```

### Recommended MVP stack

#### Mobile app options

Choose one:

- Flutter
- React Native
- Native Swift and Kotlin only if you want maximum platform-specific control and have the resources

#### Backend

Recommended simple options:

- Supabase
- Firebase

#### Database

- PostgreSQL if using Supabase
- Firestore if using Firebase

#### Storage/CDN

- Supabase Storage
- Firebase Storage
- Cloudflare R2
- Amazon S3

#### Admin dashboard

- Supabase Studio for early MVP
- Retool
- Appsmith
- Custom Next.js admin dashboard

#### Stream checker

- Node.js worker
- Python worker
- Scheduled cron job
- Cloud Run, Render, Fly.io, or Railway

#### Analytics

- Firebase Analytics
- PostHog
- Amplitude

Track:

- Station plays
- Play failures
- Buffering time
- Favorites
- Search queries
- Country/category clicks
- Subscription conversion
- Retention

---

## 7. Station Data Model

Example station record:

```json
{
  "id": "radio-city-chennai",
  "name": "Radio City 91.1 FM Chennai",
  "displayNameTamil": "ரேடியோ சிட்டி சென்னை",
  "country": "India",
  "region": "Tamil Nadu",
  "city": "Chennai",
  "language": ["Tamil"],
  "categories": ["Live FM", "Chennai", "India"],
  "type": "live_radio",
  "streamUrl": "https://example.com/live.m3u8",
  "streamFormat": "hls",
  "codec": "aac",
  "bitrate": 128,
  "homepageUrl": "https://station.example.com",
  "logoUrl": "https://your-cdn.com/logos/radio-city.png",
  "source": "official_website",
  "permissionStatus": "public_stream",
  "isFeatured": true,
  "isActive": true,
  "lastCheckedAt": "2026-05-09T00:00:00Z",
  "lastCheckStatus": "online"
}
```

Additional risk-management fields:

```json
{
  "permissionStatus": "official_partner | public_stream | user_submitted | pending_review | removed",
  "rightsNotes": "URL found on official station website",
  "takedownContact": "legal@example.com",
  "sourceEvidenceUrl": "https://station.example.com/listen-live"
}
```

---

## 8. Station Ingestion Pipeline

### Step 1: Seed collection

Collect candidates from:

- Radio Browser API
- Official station websites
- User submissions
- Manual research
- Direct station partnerships

### Step 2: Normalize data

Map every candidate into the internal station schema:

- Name
- Stream URL
- Resolved stream URL
- Country
- City
- Language
- Category
- Tags
- Logo
- Homepage
- Source
- Permission status
- Active/inactive state

### Step 3: Validate streams

A background worker should check:

- Is the URL reachable?
- Does it return an audio content type?
- Is it MP3, AAC, HLS, PLS, or M3U?
- Can it play for at least 10 to 15 seconds?
- What codec is used?
- What bitrate is available?
- Does the stream redirect?
- Is HTTPS available?
- Is it geoblocked?
- Does it return 403 for mobile clients?
- Is the stream dead?

### Step 4: Manual review

Admin should verify:

- Tamil relevance
- Correct station name
- Correct country/city
- Correct logo
- Duplicate status
- Content appropriateness
- Permission/source notes

### Step 5: Publish approved stations

Only approved, active stations should appear in the app.

### Step 6: Continuous monitoring

Every few hours:

- Check active stations
- Mark broken stations
- Try backup URLs
- Notify admin
- Remove broken stations from featured rows
- Review user reports

---

## 9. App Information Architecture

### Primary tabs

1. Home
2. Countries
3. Artists / Moods
4. Search
5. Favorites

### Home page rows

Recommended Netflix-style rows:

- Continue Listening
- Favorites
- Featured Tamil Live FM
- Chennai FM
- Sri Lanka Tamil FM
- Malaysia Tamil FM
- Singapore Tamil FM
- UAE Tamil FM
- Australia Tamil FM
- New Zealand Tamil FM
- USA Tamil FM
- Artist Radios
- 80s Tamil Hits
- 90s Tamil Hits
- 2000s Tamil Hits
- Ilaiyaraaja Radio
- A. R. Rahman Radio
- Devotional
- Instrumental
- News / Talk
- Local Tamil FM
- Trending Now

### Country page

Group by country:

- India
- Sri Lanka
- Malaysia
- Singapore
- UAE
- Australia
- New Zealand
- USA
- Canada
- UK
- Switzerland
- France
- Germany

### Artists / Moods page

Only include artist or mood categories when the content is a radio stream, not an on-demand song library.

Possible rows:

- Ilaiyaraaja Specials
- A. R. Rahman Vibes
- SPB Classics
- Yesudas Classics
- Pradeep Kumar Mood
- Tamil 80s
- Tamil 90s
- Melody
- Kuthu
- Devotional
- Instrumental
- Sleep / Relax

### Search page

Search by:

- Station name
- Artist-themed station
- Country
- City
- Mood
- Genre
- Language

### Favorites page

Support:

- Local favorites in MVP
- Cloud sync after sign-in
- Reordering favorites
- Recently played stations

---

## 10. Product Features

### MVP must-have features

- Home screen with curated rows
- Country/category browsing
- Search
- Station detail screen
- Audio player
- Mini-player
- Full-screen now-playing screen
- Favorites
- Recently played
- Sleep timer
- Background audio
- Lock screen controls
- Bluetooth controls
- AirPlay support on iOS
- Stream quality/status indicator
- Report broken station
- Request station
- Basic admin station manager
- Stream health checker
- Analytics and crash reporting

### Strong v1.5/v2 features

- User accounts
- Cloud favorites
- CarPlay
- Android Auto
- Apple Watch controls
- Siri shortcuts
- Alarm clock radio
- Station schedules
- Now-playing song metadata
- Push notifications for favorite shows
- Premium themes
- Family sharing subscription

### Avoid in MVP

- Offline downloads
- Hosted song library
- Lyrics
- Full artist database
- User uploads
- Chat/community
- Complex recommendation engine
- AI DJ
- Podcast hosting

These increase licensing, moderation, or implementation complexity.

---

## 11. Artist Radio Strategy

Artist-themed radio can be valuable, but it must be handled carefully.

### Safer approach

- Only list artist-themed stations when they are actual live/public streams.
- Do not host the artist's songs yourself.
- Do not use official artist photos without rights.
- Do not imply artist endorsement.
- Use licensed images, generated artwork, silhouettes, initials, or abstract visuals.
- Label categories as artist-inspired or station-based collections.

### Safer wording

Use:

- Ilaiyaraaja Specials
- A. R. Rahman Vibes
- SPB Classics
- Pradeep Kumar Mood
- Artist Radio Streams

Avoid:

- Official A. R. Rahman Radio, unless it is officially approved
- Official Ilaiyaraaja Station, unless it is officially approved

---

## 12. Audio Playback Requirements

### iOS

Use:

- AVPlayer
- AVAudioSession category for playback
- Background audio capability
- MPNowPlayingInfoCenter
- MPRemoteCommandCenter
- Interruption handling
- Route change handling
- Lock screen metadata
- AirPlay support

Useful reference: [Apple AVAudioSession Documentation](https://developer.apple.com/documentation/avfaudio/avaudiosession)

### Android

Use:

- ExoPlayer / Media3
- Foreground service for playback
- Media notification controls
- Audio focus handling
- Bluetooth controls
- Android Auto later

### Cross-platform options

#### Flutter

Recommended packages:

- `just_audio`
- `audio_service`
- `audio_session`

#### React Native

Recommended package:

- `react-native-track-player`

---

## 13. Monetization Strategy

The product should avoid the competitor's biggest annoyance: intrusive ads.

### Free tier

- Listen to all stations
- Favorites
- Search
- Sleep timer
- Recently played
- Basic discovery
- No intrusive full-screen ads

### Premium tier

Possible subscription benefits:

- Completely ad-free app interface
- Cloud sync
- Unlimited favorites
- Alarm clock radio
- Advanced sleep timer
- Car mode
- Custom station groups
- Premium themes
- Early access features

### Pricing idea

- Free
- $0.99/month
- $7.99/year
- $19.99 lifetime unlock, optional

The yearly plan should be the main offer.

### App Store note

If you sell digital features or app functionality on iOS, use Apple's In-App Purchase system unless a specific exception applies.

Useful reference: [Apple App Review Guidelines](https://developer.apple.com/app-store/review/guidelines/)

---

## 14. Launch Strategy

### Phase 1: Private MVP

Build with 50 to 100 verified stations.

Invite:

- Friends and family
- Tamil community members
- Tamil diaspora groups
- University Tamil associations
- Radio fans

Ask:

- Which stations are missing?
- Which streams break?
- Is playback fast?
- Is the app easier to use than competitors?
- Are country and mood categories useful?
- Would users pay for a low-cost premium version?

### Phase 2: Public beta

Use:

- TestFlight for iOS
- Internal testing or closed testing for Android

Add:

- Report broken station
- Request station
- Feedback form
- Basic analytics

### Phase 3: App Store launch

Launch with:

- 150 to 300 stations
- Strong screenshots
- Global Tamil FM positioning
- No intrusive app ads
- Optional premium subscription
- Clean onboarding
- Fast playback

### Phase 4: Station partnerships

After traction, contact stations:

> We already have Tamil listeners using our app. Would you like to claim your station profile?

Offer:

- Verified badge
- Better station logo
- Website and social links
- Featured listing opportunities
- Future listener analytics

---

## 15. Suggested Development Timeline

### Weeks 1-2: Validation and catalog

- Pick app name
- Create station schema
- Import Tamil station candidates
- Manually find official streams for top stations
- Build spreadsheet or admin table
- Validate 100 streams
- Categorize by country and type

### Weeks 3-5: App MVP

- Home screen
- Station rows
- Search
- Player
- Mini-player
- Full-screen player
- Favorites
- Recently played
- Sleep timer
- Background audio
- Report broken station

### Week 6: Backend/admin

- Station API
- Admin dashboard
- Stream checker
- Image storage
- User station request flow

### Week 7: Polish

- Better UI
- Animations
- Tamil typography
- App icon
- Onboarding
- Analytics
- Crash reporting

### Week 8: TestFlight / beta

- Invite private users
- Fix broken stations
- Improve startup speed
- Improve player reliability
- Refine categories

---

## 16. Success Metrics

Track these from the first beta:

### Activation

- First station played
- Time to first audio playback
- Onboarding completion

### Engagement

- Daily active users
- Weekly active users
- Average listening session length
- Stations played per user
- Favorites added per user
- Recently played repeat usage

### Reliability

- Stream start failure rate
- Buffering rate
- Station broken reports
- Average time to fix broken stations

### Discovery

- Search queries
- Country page clicks
- Category row clicks
- Featured station plays

### Monetization

- Free-to-premium conversion
- Trial starts
- Monthly recurring revenue
- Annual subscription conversion
- Churn

### Retention

- Day 1 retention
- Day 7 retention
- Day 30 retention

---

## 17. Final Recommended Strategy

Build a Tamil-only, premium live radio app first. Treat artist, devotional, decade, and instrumental content as radio stream categories rather than on-demand music libraries.

The winning formula:

```text
Tamil-only niche
+ global country categories
+ no annoying in-app ads
+ beautiful Netflix-style discovery
+ reliable playback
+ fast station maintenance
+ user requests
+ station owner claim/removal flow
+ low-cost premium subscription
= strong chance of success
```

### First version should be

> A premium Tamil live radio directory and player for worldwide Tamil listeners.

### Source stations through

1. Radio Browser API as seed data
2. Official station websites
3. User submissions with moderation
4. Direct station partnerships

### Reduce risk through

- Direct stream playback
- No hosted song files
- No competitor content copying
- No unlicensed artist photos
- Clear attribution
- Takedown and claim flow
- Source evidence tracking
- Gradual movement toward official station permissions

### Win users through

- No intrusive app ads
- Better design
- Faster playback
- Better categories
- Favorites and recently played
- Sleep timer
- Car, Bluetooth, and lock screen support
- Reliable station repair
