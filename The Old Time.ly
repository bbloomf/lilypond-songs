\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"The Old Time"}}
  composer = \markup\oldStyleNum"J. R. Thomas, 1873"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \key ees \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	\partial 8
  ees8 |
  g4 f8 g4 aes8 |
  bes4. c8 bes4 |
  ees,8 d ees aes4 g8 |
  f4.~ f4 \bar"" d8 |
  
  %page2
  ees4 f8 g4 aes8 |
  bes4 c8 bes4 bes16[ c] |
  d4 bes8 a4 c8 |
  bes4.~ bes4 \bar"" bes8 |
  
  bes4 aes8 aes4 aes8 |
  g4 bes8 ees4 d8 |
  c4 bes8 aes4 g8 |
  aes4.~ aes4 \bar"" c8 |
  
  c4 bes8 aes4 g8 |
  g4 f8 ees4 es8 |
  g4 f8 ees4 d8 |
  ees4.~ ees4 \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	’Twas when the hay was mown, Mag -- gie,
  In the long years a -- go, __
  And while the wes -- tern sky was rich
  With sun -- set’s ros -- y glow, __
  Then hand in hand close linked we passed
  The dew -- y ricks be -- tween,
  When I was one and twen -- ty, Mag,
  And you were sev -- en -- teen. __
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  Your voice was low and sweet, Mag -- gie,
  Your wav -- y hair was brown,
  Your cheek was like the wild red rose
  That show’rs its pet -- als down; __
  Your eyes were like the blue speed -- well
  With dew -- y mois -- ture sheen, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  The years have come and gone, Mag -- gie,
  With sun -- shine and with shade, __
  And sil -- vered is the silk -- en hair
  That o’er your shoul -- ders strayed,
  In ma -- ny~a soft and way -- ward tress,
  The fair -- est ev -- er seen, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
  Though gen -- tly chang -- ing time, Mag -- gie,
  Has touched you in his flight, __
  Your voice has still the old sweet tone,
  Your eyes the old love light, __
  And years can nev -- er, nev -- er change,
  The heart you gave, I ween, __
  %When I was one and twen -- ty, Mag,
  %And you were sev -- en -- teen. __
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  ees8 |
  
  ees4 d8 ees4 f8 |
  g4. ees8 g4 |
  c,8 c c ees4 ees8 |
  d4.~ d4 d8 |
  
  %page2
  bes4 bes8 ees4 f8 |
  g4 g8 g4 g8 |
  bes4 bes8 ees,4 ees8 |
  d4.~ d4 d8 |
  
  d4 d8 d4 bes8 |
  ees4 g8 g4 g8 |
  ees4 ees8 f4 e8 |
  f4.~ f4 aes8 |
  
  aes4 f8 d4 ees8 |
  d4 d8 c4 c8 |
  c4 c8 bes4 bes8 |
  bes4.~ bes4 \bar"|."
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
  ees,8 |
  bes'4 bes8 bes4 bes8 |
  bes4. aes8 bes4 |
  g8 g g c4 bes8 |
  bes4.~ bes4 aes8 |
  
  %page2
  g4 g8 bes4 bes8 |
  bes4 ees8 ees4 ees8 |
  d4 d8 c4 a8 |
  bes4.~ bes4 bes8 |
  
  f4 f8 bes4 bes8 |
  bes4 bes8 bes4 b8 |
  c4 bes?8 c4 c8 |
  c4.~ c4 ees8 |
  
  bes4 aes8 f4 bes8 |
  b4 b8 g4 g8 |
  ees4 ees8 f4 f8 |
  g4.~ g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  ees,8 |
  ees4 bes8 ees4 d8 |
  ees4. ees8 ees4 |
  c8 c c aes4 g8 |
  bes4.~ bes4 bes8 |
  
  %page2
  ees4 ees8 ees4 ees8 |
  ees4 ees8 ees4 ees8 |
  f4 f8 f,4 f8 |
  bes4.~ bes4 bes8 |
  
  bes4 bes8 bes4 d8 |
  ees4 ees8 ees4 g8 |
  aes4 g8 f4 c8 |
  f4.( ees4) ees8 |
  
  d4 d8 bes4 ees8 |
  g4 g8 c,4 c8 |
  aes4 aes8 bes4 bes8 |
  ees4.~ ees4 \bar"|."
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
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 105
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
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
}


