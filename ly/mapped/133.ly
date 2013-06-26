\version "2.14.2"
\include "util.ly"
\header{ tagline = ""}
\paper {
  print-all-headers = ##t
  ragged-right = ##f
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -3)
       (stretchability . 100))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #133
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key d \major
  \time 3/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 4
  \repeat unfold 2 {
    fis8 e |
    d4 d fis8 a |
    e4 e fis8 a |
    b4 a fis8 e |
    d2 \bar""
  }
  
  a'8 b16[ cis] |
  d4 cis b8 a |
  b[ a] fis4 a8 b16[ cis] |
  d4 cis b8 a |
  d2 \bar ""

  fis,8 e |
  d4 d fis8 a |
  e4 e fis8 a |
  b4 a fis8 e |
  d2 \bar""
  
  \bar "|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Come, Thou Fount of eve -- ry bless -- ing,
    Tune my heart to sing Thy grace;
  Streams of mer -- cy, nev -- er ceas -- ing,
    Call for songs of loud -- est praise.
  Teach me some mel -- o -- dious son -- net,
    Sung by flam -- ing tongues a -- bove.
  Praise the mount! I’m fixed up -- on it,
    Mount of God’s un -- chang -- ing love.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Sor -- rowing I shall be in spi -- rit,
    Till re -- leased from flesh and sin,
  Yet from what I do in -- her -- it,
    Here Thy prais -- es I’ll be -- gin;
  Here I raise my Eb -- e -- nee -- zer;
    Here by Thy great help I’ve come;
  And I hope, by Thy good plea -- sure,
    Safe -- ly to ar -- rive at home.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Je -- sus sought me when a stran -- ger,
    Wan -- d’ring from the fold of God;
  He, to res -- cue me from dan -- ger,
    In -- ter -- posed His pre -- cious blood;
  How His kind -- ness yet pur -- sues me
    Mor -- tal tongue can nev -- er tell,
  Clothed in flesh, till death shall loose me
    I can -- not pro -- claim it well.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  O to grace how great a debt -- or
    Dai -- ly I’m con -- strained to be!
  Let Thy good -- ness, like a fet -- ter,
    Bind my wan -- d’ring heart to Thee.
  Prone to wan -- der, Lord, I feel it,
    Prone to leave the God I love;
  Here’s my heart, O take and seal it,
    Seal it for Thy courts a -- bove.
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
  O that day when freed from sin -- ning,
    I shall see Thy love -- ly face;
  Cloth -- ed then in blood washed lin -- en
    How I’ll sing Thy sov -- ’reign grace;
  Come, my Lord, no long -- er tar -- ry,
    Take my ran -- somed soul a -- way;
  Send Thine an -- gels now to car -- ry
    Me to realms of end -- less day.
}

altoMusic = \relative c' {
  \partial 4
  \repeat unfold 2 {
    d8 cis |
    d4 d d8 d |
    cis4 cis d8 d |
    d4 d d8 cis |
    d2 \bar""
  }
  
  d8 g |
  fis4 a g8 fis |
  d4 d d8 g |
  fis4 a g8 fis |
  fis2 \bar""
  
  d8 cis |
  d4 d d8 d |
  cis4 cis d8 d |
  d4 d d8 cis |
  d2
  
  \bar "|."
}
altoWords = \lyricmode {
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
  \partial 4
  \repeat unfold 2 {
    a8 g |
    fis4 fis a8 a |
    a4 a a8 a |
    g4 fis a8 g |
    fis2 \bar""
  }
  
  a8 g |
  a4 a d8 d |
  g,8[ fis] a4 a8 g |
  a4 a d8 d |
  a2 \bar""
  
  a8 g |
  fis4 fis a8 a |
  a4 a a8 a |
  g4 fis a8 g |
  fis2
  
  \bar "|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \partial 4
  \repeat unfold 2 {
    d,8 a |
    d4 d d8 fis, |
    a4 a d8 fis |
    g4 d a8 a |
    d2 \bar""
  }
  
  fis8 e |
  d4 fis g8 d |
  d4 d fis8 e |
  d4 fis g8 d |
  d2 \bar""
  
  d8 a |
  d4 d d8 fis, |
  a4 a d8 fis |
  g4 d a8 a |
  d2
  
  \bar "|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" \transpose d ees{ \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" \transpose d ees{ \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "altos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "altos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "altos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "altos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "altos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" \transpose d ees{ \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" \transpose d ees{ \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  
  \layout {
    \context {
      \Lyrics
      \override LyricText #'font-size = #1.3
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Come, Thou Fount of Every Blessing"}}
  poet = "Robert Robinson (1735–1790)"
  composer = \markup\oldStyleNum\concat{"from " \italic"Repository of Sacred Music, Part Second" ", 1813"}
  tagline = ""
}}



